import json, glob, re

# tension/controversy notes for the weaves that have one (exact, sober prose)
tensions = {
 "chronicler-david.json": "Chronicles reads \"Satan provoked David\" where Samuel reads \"the LORD moved David\" (1 Chr 21:1 / 2 Sam 24:1); the census totals (1 Chr 21:5 / 2 Sam 24:9) and the price of Ornan's floor (1 Chr 21:25 / 2 Sam 24:24) differ; and the Chronicler omits Bathsheba, Uriah, and Nathan's rebuke entirely.",
 "davids-mighty-men.json": "The chief's name and feat disagree — Jashobeam against three hundred (1 Chr 11:11), the Tachmonite, Adino the Eznite, against eight hundred (2 Sam 23:8) — and the tail of the roster diverges in names and order, with sixteen warriors found only in Chronicles.",
 "the-spies-recounted.json": "Deuteronomy lays the request for spies on the people (\"We will send men,\" Deut 1:22) where Numbers makes it the LORD's command (Num 13:1-2) — a genuine tension over who proposed the mission.",
 "the-wife-as-a-sister.json": "Whether these are three separate incidents or variant tellings of one tradition is debated, and the patriarchs' deception is a moral difficulty the text leaves unresolved.",
 "the-olivet-discourse.json": "Luke replaces Matthew and Mark's \"abomination of desolation\" with \"Jerusalem compassed with armies\" (Luke 21:20), and \"This generation shall not pass\" (Matt 24:34) is read very differently by preterist and futurist interpreters.",
 "the-fool-hath-said.json": "Psalm 53 compresses and rewords Psalm 14:5-6 into a single, differently-worded verse (53:5) and says \"God\" where Psalm 14 says \"the LORD\"; whether they are one psalm or two is debated.",
 "naaman-in-the-jordan.json": "This is a typological reading, not a parallel the text states; the tie between Naaman's cleansing and Christ's baptism is interpretive.",
 "the-two-creation-accounts.json": "Whether Genesis 1 and 2 are one account told twice or two sources is the classic source-critical debate; read traditionally, Genesis 2 returns to the sixth day up close.",
 "the-fall-of-jerusalem.json": "2 Chronicles 36:17 (the slaughter in the sanctuary) has no verse-level counterpart in 2 Kings 25 or Jeremiah 52 and is left unlinked.",
 "swords-into-plowshares.json": "Whether Isaiah quotes Micah, Micah quotes Isaiah, or both draw on an older oracle is unsettled.",
 "davids-song-twice.json": "The two copies differ in wording at many points (2 Samuel 22 splits the rock-and-fortress imagery that Psalm 18 merges), reflecting separate transmission.",
}

for path in sorted(glob.glob("weaves/*.json")):
    fn = path.replace("\\","/").split("/")[-1]
    with open(path, encoding="utf-8") as f:
        text = f.read()
    lines = text.split("\n")
    out = []
    for line in lines:
        out.append(line)
        m = re.match(r'^(\s*)"created":', line)
        if m:
            indent = m.group(1)
            if '"approved"' not in text:
                out.append(f'{indent}"approved": false,')
            if fn in tensions and '"tension"' not in text:
                out.append(f'{indent}"tension": {json.dumps(tensions[fn], ensure_ascii=False)},')
    new = "\n".join(out)
    if new != text:
        with open(path, "w", encoding="utf-8", newline="\n") as f:
            f.write(new)
        print(f"updated {fn}")
    else:
        print(f"unchanged {fn}")
