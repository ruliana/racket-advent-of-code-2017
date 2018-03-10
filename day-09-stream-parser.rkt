#lang brag
stream : group | garbage | text
group : OPEN-GROUP @stream* CLOSE-GROUP
garbage: OPEN-GARBAGE /waste* CLOSE-GARBAGE
waste: OPEN-GARBAGE | CLOSE-GROUP | OPEN-GROUP | CHAR
@text: CHAR+
