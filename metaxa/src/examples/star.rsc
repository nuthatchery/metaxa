/*
Supposed to be a language which accepts strings of this form:
an arbitary long sequence of whitespace separated "words", and these words are just defined lexographically as being close to
how identifiers in many programming languages are defined.
*/
module examples::star

start syntax Rep = Id*;

// the following code taken/adapted from MetaXa.rsc.
lexical Id = [a-zA-Z_] !<< [a-zA-Z_] [a-zA-Z_0-9]* !>> [a-zA-Z_0-9];

layout Layout = Layout: Space* !>> [\ \t\n\r\f] !>> "//";

lexical Space = [\ \t\n\r\f];