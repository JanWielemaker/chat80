# The classical CHAT80 natural language system

The CHAT80 system has been developed in the 70s and 80s by Fernando C.N.
Pereira and David H.D. Warren. It implements a natural language question
answering system that answers  questions   about  the  world: countries,
cities, rivers, etc. It does so by   parsing the question, translate the
parse to a Prolog query and run this against its database.

This version is derived from the original  via Quintus Prolog after some
compatibility modifications for SWI-Prolog and   adding  a module header
that allows using it safely together with other applications.

The code is definitely dated. Still, it   provides  a nice example using
Prolog for parsing, assigning meaning and querying.

## Legal

On August 10 2020, the code was re-licensed on request from the original
authors David H. D. Warren and Fernando   C.  N. Pereira on request from
Vijay Saraswat.  CHAT-80 is now available under the MIT license.

