
HDprob <- prior.CPT("HD", "1", "0", .4)
HPprob <- prior.CPT("HP", "1", "0", .02)

Eprob <- doubleCPT("E", "HD", "HP",  probEifH1S1H2S1 = .8, probEifH1S1H2S2 =.6, probEifH1S2H2S1 =.7 , probEifH1S2H2S2 = .05)


STMpr <- list(HD = HDprob, HP = HPprob, E = Eprob)

STMbn <- custom.fit(STMdag, STMpr)

CPkable0("STMbn", "HD")
CPkable0("STMbn", "HP")
CPkable2("STMbn", "E")



STMjn <- compile(as.grain(STMbn))

STMHD <-  setEvidence(STMjn, nodes = "HD", states = "1")
STMnoHD <- setEvidence(STMjn, nodes = "HD", states = "0")


PReHD <- querygrain(STMHD , nodes = "E")[[1]][1]
PRenHD <- querygrain(STMnoHD , nodes = "E")[[1]][1]

LRHD <- PReHD/PRenHD

STMHP <-  setEvidence(STMjn, nodes = "HP", states = "1")
STMnoHP <- setEvidence(STMjn, nodes = "HP", states = "0")

PReHP <- querygrain(STMHP , nodes = "E")[[1]][1]
PRenHP <- querygrain(STMnoHP , nodes = "E")[[1]][1]

LRHP <- PReHP/PRenHP


