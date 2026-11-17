"""Herpetodados: app local (só neste computador, 127.0.0.1).

Serve a página inicial (inserir planilhas, lista de levantamentos, guia), o painel de cada levantamento
e uma pequena API que roda o R:
  POST /api/enviar?nome=<arquivo.xlsx>     corpo = bytes da planilha -> cria levantamento e roda as análises
  GET  /api/levantamentos                  lista com o resumo de cada um
  GET  /api/tarefa/<id>                    andamento de uma tarefa (log do R)
  POST /api/l/<id>/estilo                  grava estilo.json e refaz só as figuras do R
  POST /api/l/<id>/contexto                grava contexto_campo.txt
  POST /api/l/<id>/leitura                 grava leitura_especialista.md
  POST /api/l/<id>/refazer                 roda as análises de novo (mesma planilha)
  POST /api/l/<id>/planilha?nome=          troca a planilha pela versão corrigida (guarda a anterior) e roda de novo
  POST /api/l/<id>/pasta                   abre a pasta do levantamento no Explorer
Só biblioteca padrão do Python. Rodar com:  python -I app/servidor.py
"""
import json, os, re, shutil, subprocess, sys, threading, time, unicodedata, uuid, webbrowser
from http.server import ThreadingHTTPServer, SimpleHTTPRequestHandler
from urllib.parse import urlparse, parse_qs, unquote

RAIZ = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LEV = os.path.join(RAIZ, "levantamentos")
def acha_rscript():
    """Rscript: variável HERP_RSCRIPT, o R mais novo em Arquivos de Programas, ou o que estiver no PATH."""
    import glob
    if os.environ.get("HERP_RSCRIPT") and os.path.exists(os.environ["HERP_RSCRIPT"]):
        return os.environ["HERP_RSCRIPT"]
    cands = []
    for base in [os.environ.get("ProgramFiles", r"C:\Program Files"), os.environ.get("LOCALAPPDATA", "") + r"\Programs"]:
        cands += glob.glob(os.path.join(base, "R", "R-*", "bin", "x64", "Rscript.exe")) + glob.glob(os.path.join(base, "R", "R-*", "bin", "Rscript.exe"))
    def versao(c):
        m = re.search(r"R-(\d+)\.(\d+)\.(\d+)", c)
        return tuple(int(x) for x in m.groups()) if m else (0,)
    if cands:
        return sorted(cands, key=lambda c: (versao(c), "x64" in c))[-1]
    return shutil.which("Rscript") or "Rscript"


RSCRIPT = acha_rscript()
PORTA = int(os.environ.get("HERP_PORTA", "8810"))
MAX_UPLOAD = 60 * 1024 * 1024
os.makedirs(LEV, exist_ok=True)

TAREFAS = {}
TRAVA = threading.Lock()


def slug(nome):
    s = unicodedata.normalize("NFKD", nome).encode("ascii", "ignore").decode()
    s = re.sub(r"\.(xlsx|xls)$", "", s, flags=re.I)
    s = re.sub(r"[^A-Za-z0-9]+", "-", s).strip("-").lower()[:50] or "levantamento"
    base, i = s, 2
    while os.path.exists(os.path.join(LEV, s)):
        s = f"{base}-{i}"; i += 1
    return s


def pasta_ok(lid):
    if not re.fullmatch(r"[a-z0-9-]{1,60}", lid or ""):
        return None
    p = os.path.join(LEV, lid)
    return p if os.path.isdir(p) else None



def xlsx_simples(caminho, abas):
    """Grava um .xlsx só com a biblioteca padrão. abas = [(nome, [linha0 (cabeçalho), linha1, ...]), ...].
    Números escritos como "12", "0,53" ou "42%" viram números de verdade (a % com formato de porcentagem)."""
    import zipfile
    from xml.sax.saxutils import escape

    def col(i):
        n = ""
        i += 1
        while i:
            i, r = divmod(i - 1, 26); n = chr(65 + r) + n
        return n

    def celula(ref, v, cab):
        t = "" if v is None else str(v).strip()
        if cab:
            return f'<c r="{ref}" t="inlineStr" s="2"><is><t xml:space="preserve">{escape(t)}</t></is></c>'
        m = re.fullmatch(r"(-?\d{1,3}(?:\.\d{3})+|-?\d+)(,\d+)?(%)?", t)
        if m and t not in ("-", ""):
            num = float(m.group(1).replace(".", "") + (m.group(2) or "").replace(",", "."))
            if m.group(3):
                return f'<c r="{ref}" s="{3 if m.group(2) else 1}"><v>{num / 100}</v></c>'
            return f'<c r="{ref}"><v>{num}</v></c>'
        return f'<c r="{ref}" t="inlineStr"><is><t xml:space="preserve">{escape(t)}</t></is></c>'

    usados, nomes = set(), []
    for nome, _ in abas:
        n = re.sub(r"[\[\]\*\?/\\:]", " ", nome)[:31].strip() or "Tabela"
        base, k = n, 2
        while n.lower() in usados:
            n = f"{base[:28]} {k}"; k += 1
        usados.add(n.lower()); nomes.append(n)
    with zipfile.ZipFile(caminho, "w", zipfile.ZIP_DEFLATED) as z:
        z.writestr("[Content_Types].xml", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">'
                   '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/>'
                   '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>'
                   '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>'
                   + "".join(f'<Override PartName="/xl/worksheets/sheet{i + 1}.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>' for i in range(len(abas)))
                   + "</Types>")
        z.writestr("_rels/.rels", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
                   '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>')
        z.writestr("xl/workbook.xml", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" '
                   'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets>'
                   + "".join(f'<sheet name="{escape(n)}" sheetId="{i + 1}" r:id="rId{i + 1}"/>' for i, n in enumerate(nomes)) + "</sheets></workbook>")
        z.writestr("xl/_rels/workbook.xml.rels", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
                   + "".join(f'<Relationship Id="rId{i + 1}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet{i + 1}.xml"/>' for i in range(len(abas)))
                   + f'<Relationship Id="rId{len(abas) + 1}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/></Relationships>')
        z.writestr("xl/styles.xml", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">'
                   '<fonts count="2"><font><sz val="11"/><name val="Calibri"/></font><font><b/><sz val="11"/><name val="Calibri"/></font></fonts>'
                   '<fills count="3"><fill><patternFill patternType="none"/></fill><fill><patternFill patternType="gray125"/></fill><fill><patternFill patternType="solid"><fgColor rgb="FFE9E4D6"/></patternFill></fill></fills>'
                   '<borders count="1"><border/></borders><cellStyleXfs count="1"><xf/></cellStyleXfs>'
                   '<cellXfs count="4"><xf/><xf numFmtId="9" applyNumberFormat="1"/><xf fontId="1" fillId="2" applyFont="1" applyFill="1"/><xf numFmtId="10" applyNumberFormat="1"/></cellXfs></styleSheet>')
        for i, (_, linhas) in enumerate(abas):
            larg = {}
            for ln in linhas:
                for j, v in enumerate(ln):
                    larg[j] = min(60, max(larg.get(j, 6), len(str(v if v is not None else "")) + 2))
            cols = "<cols>" + "".join(f'<col min="{j + 1}" max="{j + 1}" width="{w}" customWidth="1"/>' for j, w in sorted(larg.items())) + "</cols>" if larg else ""
            rows = "".join(f'<row r="{r + 1}">' + "".join(celula(f"{col(j)}{r + 1}", v, r == 0) for j, v in enumerate(ln)) + "</row>" for r, ln in enumerate(linhas))
            z.writestr(f"xl/worksheets/sheet{i + 1}.xml", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">'
                       '<sheetViews><sheetView workbookViewId="0"><pane ySplit="1" topLeftCell="A2" activePane="bottomLeft" state="frozen"/></sheetView></sheetViews>'
                       f'{cols}<sheetData>{rows}</sheetData></worksheet>')
    return nomes

def rodar(cmd, lid, rotulo, env_extra=None):
    tid = uuid.uuid4().hex[:10]
    TAREFAS[tid] = {"id": tid, "levantamento": lid, "rotulo": rotulo, "estado": "rodando", "log": [], "inicio": time.time(), "pct": 0, "etapa": "Começando"}

    def trabalho():
        t = TAREFAS[tid]
        env = dict(os.environ, **(env_extra or {}))
        try:
            p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, cwd=RAIZ, env=env,
                                 creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
            for linha in iter(p.stdout.readline, b""):
                txt = linha.decode("utf-8", "replace").rstrip()
                m = re.match(r"\[\s*(\d+)%\] (.*)", txt) or re.match(r"@@ (\d+) (.*)", txt)
                if m:  # linha de andamento enviada pelo R: "[ 45%] <etapa>"
                    t["pct"] = min(100, int(m.group(1))); t["etapa"] = m.group(2)
                elif txt and "permutations" not in txt and "minperm" not in txt:
                    t["log"].append(txt)
            p.wait()
            t["estado"] = "pronto" if p.returncode == 0 else "erro"
            if p.returncode == 0: t["pct"] = 100; t["etapa"] = "Pronto"
        except Exception as e:  # noqa: BLE001
            t["log"].append(f"Falha ao rodar o R: {e}"); t["estado"] = "erro"
        with open(os.path.join(LEV, lid, "log.txt"), "a", encoding="utf-8") as f:
            f.write(f"\n== {time.strftime('%d/%m/%Y %H:%M')} {rotulo} ({t['estado']})\n" + "\n".join(t["log"]) + "\n")
    threading.Thread(target=trabalho, daemon=True).start()
    return tid


def analisar(lid):
    p = os.path.join(LEV, lid)
    meta = json.load(open(os.path.join(p, "origem.json"), encoding="utf-8"))
    return rodar([RSCRIPT, os.path.join(RAIZ, "R", "herpetodados.R"), os.path.join(p, "planilha.xlsx"), p], lid,
                 "Análises", {"HERP_NOME": meta.get("nome", "planilha.xlsx")})


def listar():
    out = []
    for lid in sorted(os.listdir(LEV)):
        p = os.path.join(LEV, lid)
        if not os.path.isdir(p):
            continue
        item = {"id": lid}
        for arq, chave in (("origem.json", "origem"), ("resumo.json", "resumo")):
            f = os.path.join(p, arq)
            if os.path.exists(f):
                try:
                    item[chave] = json.load(open(f, encoding="utf-8"))
                except Exception:  # noqa: BLE001
                    pass
        item["tem_painel"] = os.path.exists(os.path.join(p, "dados.js"))
        item["rodando"] = any(t["levantamento"] == lid and t["estado"] == "rodando" for t in TAREFAS.values())
        out.append(item)
    out.sort(key=lambda x: (x.get("origem") or {}).get("enviado_em", ""), reverse=True)
    return out


class Manipulador(SimpleHTTPRequestHandler):
    def __init__(self, *a, **k):
        super().__init__(*a, directory=RAIZ, **k)

    def log_message(self, *a):
        pass

    def end_headers(self):
        self.send_header("Cache-Control", "no-store")
        super().end_headers()

    def json(self, obj, cod=200):
        b = json.dumps(obj, ensure_ascii=False).encode("utf-8")
        self.send_response(cod); self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(b))); self.end_headers(); self.wfile.write(b)

    def corpo(self):
        n = int(self.headers.get("Content-Length") or 0)
        if n > MAX_UPLOAD:
            raise ValueError("arquivo grande demais")
        return self.rfile.read(n)

    def do_GET(self):
        u = urlparse(self.path); caminho = unquote(u.path)
        if caminho in ("/", "/index.html"):
            self.path = "/app/inicio.html"; return super().do_GET()
        if caminho == "/api/levantamentos":
            return self.json(listar())
        m = re.fullmatch(r"/api/tarefa/([a-f0-9]{10})", caminho)
        if m:
            t = TAREFAS.get(m.group(1))
            return self.json(t if t else {"estado": "desconhecida"}, 200 if t else 404)
        m = re.fullmatch(r"/l/([a-z0-9-]+)/?(painel\.html)?", caminho)
        if m:  # o painel é sempre o modelo atual, lendo o dados.js da pasta do levantamento
            if not pasta_ok(m.group(1)):
                return self.send_error(404)
            self.path = "/painel/painel.html"; return super().do_GET()
        m = re.fullmatch(r"/l/([a-z0-9-]+)/(.+)", caminho)
        if m:
            if not pasta_ok(m.group(1)) or ".." in m.group(2):
                return self.send_error(404)
            self.path = "/levantamentos/" + m.group(1) + "/" + m.group(2); return super().do_GET()
        if caminho.startswith(("/app/", "/painel/", "/identidade/")):
            return super().do_GET()
        return self.send_error(404)

    def do_POST(self):
        u = urlparse(self.path); caminho = unquote(u.path)
        if self.headers.get("Origin") not in (None, f"http://127.0.0.1:{PORTA}", f"http://localhost:{PORTA}"):
            return self.send_error(403)
        try:
            if caminho == "/api/enviar":
                nome = os.path.basename(parse_qs(u.query).get("nome", ["planilha.xlsx"])[0])
                if not re.search(r"\.(xlsx|xls)$", nome, re.I):
                    return self.json({"erro": "Envie uma planilha .xlsx ou .xls."}, 400)
                dados = self.corpo()
                if dados[:2] != b"PK" and dados[:4] != b"\xd0\xcf\x11\xe0":
                    return self.json({"erro": "O arquivo não parece ser uma planilha do Excel."}, 400)
                with TRAVA:
                    lid = slug(nome); p = os.path.join(LEV, lid); os.makedirs(p)
                with open(os.path.join(p, "planilha.xlsx"), "wb") as f:
                    f.write(dados)
                json.dump({"nome": nome, "enviado_em": time.strftime("%Y-%m-%d %H:%M:%S"), "titulo": re.sub(r"\.(xlsx|xls)$", "", nome, flags=re.I)},
                          open(os.path.join(p, "origem.json"), "w", encoding="utf-8"), ensure_ascii=False)
                return self.json({"id": lid, "tarefa": analisar(lid)})
            m = re.fullmatch(r"/api/l/([a-z0-9-]+)/(estilo|contexto|leitura|refazer|pasta|titulo|selecao|relatorio|planilha)", caminho)
            if not m:
                return self.send_error(404)
            lid, acao = m.groups(); p = pasta_ok(lid)
            if not p:
                return self.send_error(404)
            if acao == "pasta":
                sub = parse_qs(u.query).get("sub", [""])[0]
                alvo = os.path.join(p, sub) if sub in ("relatorio", "figuras", "tabelas") and os.path.isdir(os.path.join(p, sub)) else p
                os.startfile(alvo); return self.json({"ok": True})
            if acao == "selecao":
                sel = json.loads(self.corpo().decode("utf-8"))
                json.dump(sel, open(os.path.join(p, "selecao.json"), "w", encoding="utf-8"), ensure_ascii=False)
                return self.json({"ok": True})
            if acao == "relatorio":
                sel = json.loads(self.corpo().decode("utf-8"))
                rel = os.path.join(p, "relatorio"); shutil.rmtree(rel, ignore_errors=True); os.makedirs(rel)
                nfig = ntab = 0; leia = ["Itens do relatório (na ordem escolhida no painel)", ""]
                abas = []; indice = [["Aba", "Tabela"]]
                limpa = lambda t: re.sub(r"[^A-Za-z0-9]+", "_", unicodedata.normalize("NFKD", t).encode("ascii", "ignore").decode()).strip("_")[:60]
                for it in sel:
                    if it.get("tipo") == "fig":
                        orig = os.path.normpath(os.path.join(p, it.get("arquivo", "")))
                        if not orig.startswith(p) or not os.path.exists(orig):
                            continue
                        nfig += 1; nome = f"Fig{nfig:02d}_{limpa(it.get('titulo', 'figura'))}.png"
                        shutil.copyfile(orig, os.path.join(rel, nome)); leia.append(f"Figura {nfig}. {it.get('titulo', '')}  ->  {nome}")
                    elif it.get("tipo") == "tab":
                        ntab += 1
                        abas.append((f"Tab{ntab:02d} {it.get('titulo', 'tabela')}", [it.get("cols", [])] + it.get("linhas", [])))
                        indice.append([f"Tab{ntab:02d}", it.get("titulo", "")])
                        leia.append(f"Tabela {ntab}. {it.get('titulo', '')}  ->  aba Tab{ntab:02d} de Tabelas_do_relatorio.xlsx")
                if abas:
                    xlsx_simples(os.path.join(rel, "Tabelas_do_relatorio.xlsx"), [("Índice", indice)] + abas)
                open(os.path.join(rel, "LEIA-ME.txt"), "w", encoding="utf-8").write("\n".join(leia) + "\n\nTodas as tabelas estão numa planilha só (Tabelas_do_relatorio.xlsx), uma aba por tabela; a aba Índice lista todas.\n")
                return self.json({"figuras": nfig, "tabelas": ntab, "pasta": rel})
            if acao == "planilha":
                nome = os.path.basename(parse_qs(u.query).get("nome", ["planilha.xlsx"])[0])
                dados = self.corpo()
                if not re.search(r"\.(xlsx|xls)$", nome, re.I) or (dados[:2] != b"PK" and dados[:4] != b"\xd0\xcf\x11\xe0"):
                    return self.json({"erro": "Envie uma planilha do Excel (.xlsx)."}, 400)
                atual = os.path.join(p, "planilha.xlsx")
                if os.path.exists(atual):
                    shutil.copyfile(atual, os.path.join(p, time.strftime("planilha_anterior_%Y%m%d_%H%M%S.xlsx")))
                with open(atual, "wb") as f:
                    f.write(dados)
                try:
                    o = json.load(open(os.path.join(p, "origem.json"), encoding="utf-8"))
                except Exception:  # noqa: BLE001
                    o = {}
                o.update({"nome": nome, "atualizado_em": time.strftime("%Y-%m-%d %H:%M:%S")})
                json.dump(o, open(os.path.join(p, "origem.json"), "w", encoding="utf-8"), ensure_ascii=False)
                return self.json({"tarefa": analisar(lid)})
            if acao == "refazer":
                return self.json({"tarefa": analisar(lid)})
            corpo = self.corpo().decode("utf-8")
            if acao == "estilo":
                json.loads(corpo)
                open(os.path.join(p, "estilo.json"), "w", encoding="utf-8").write(corpo)
                if not os.path.exists(os.path.join(p, "resultados.rds")):
                    return self.json({"tarefa": analisar(lid)})
                so = parse_qs(u.query).get("so", [""])[0]
                so = so if re.fullmatch(r"[a-z_]{1,40}", so) else ""
                return self.json({"tarefa": rodar([RSCRIPT, os.path.join(RAIZ, "R", "figuras.R"), p] + ([so] if so else []), lid,
                                                  "Figura " + so if so else "Figuras")})
            if acao == "contexto":
                j = json.loads(corpo)
                open(os.path.join(p, "contexto_campo.txt"), "w", encoding="utf-8").write(
                    "COMO FOI O CAMPO\n" + j.get("campo", "") + "\n\nFINALIDADE\n" + j.get("finalidade", "") + "\n")
                json.dump(j, open(os.path.join(p, "contexto.json"), "w", encoding="utf-8"), ensure_ascii=False)
                return self.json({"ok": True})
            if acao == "leitura":
                open(os.path.join(p, "leitura_especialista.md"), "w", encoding="utf-8").write(corpo)
                return self.json({"ok": True})
            if acao == "titulo":
                j = json.loads(corpo); f = os.path.join(p, "origem.json"); o = json.load(open(f, encoding="utf-8"))
                o["titulo"] = j.get("titulo", o.get("titulo", ""))[:120]
                json.dump(o, open(f, "w", encoding="utf-8"), ensure_ascii=False); return self.json({"ok": True})
        except (ValueError, json.JSONDecodeError) as e:
            return self.json({"erro": str(e)}, 400)


def main():
    srv = ThreadingHTTPServer(("127.0.0.1", PORTA), Manipulador)
    url = f"http://127.0.0.1:{PORTA}/"
    print(f"Herpetodados aberto em {url}  (feche esta janela para encerrar)")
    if "--sem-navegador" not in sys.argv:
        threading.Timer(0.8, lambda: webbrowser.open(url)).start()
    srv.serve_forever()


if __name__ == "__main__":
    main()
