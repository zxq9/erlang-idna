## erlang-idna

A pure Erlang IDNA implementation.


## Usage

```erlang
1> application:ensure_all_started(idna).
{ok,[p1_utils,stringprep,idna]}
2> Domain = <<"www.詹姆斯.com"/utf8>>.
<<119,119,119,46,232,169,185,229,167,134,230,150,175,46,
  99,111,109>>
3> idna:to_ascii(Domain).
"www.xn--8ws00zhy3a.com"
4> idna:to_unicode("www.xn--8ws00zhy3a.com").
<<119,119,119,46,232,169,185,229,167,134,230,150,175,46,
  99,111,109>>
```


## Useful references

[RFC3490](http://www.ietf.org/rfc/rfc3490.txt) (IDNA)

[RFC3492](http://www.ietf.org/rfc/rfc3492.txt) (Punycode)

[addressable](http://github.com/sporkmonger/addressable) (Ruby URI implementation)

[punycode4r](http://raa.ruby-lang.org/project/punycode4r/) (Ruby punycode implementation)

[Unicode Character Database](http://www.unicode.org/Public/UNIDATA/UCD.html)

[UAX #15](http://www.unicode.org/reports/tr15/) (Unicode Normalization Forms)
