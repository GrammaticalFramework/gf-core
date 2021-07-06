--# -path=.:present

concrete PhrasebookGer of Phrasebook =
  open
    SyntaxGer,
    LexiconGer in {

  lincat
    VerbPhrase  = VP ;

  lin
    VRead = mkVP <lin V read_V2 : V> ;

}
