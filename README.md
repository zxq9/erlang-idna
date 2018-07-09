## erlang-idna

A pure Erlang IDNA implementation.

* support IDNA 2008


## Usage

```erlang
1> idna:to_ascii("日本語。ＪＰ").
"xn--wgv71a119e.xn--jp-"
2> idna:to_ascii("日本語.ＪＰ").
"xn--wgv71a119e.xn--jp-"
...
```


## Useful references

[RFC3490](http://www.ietf.org/rfc/rfc3490.txt) (IDNA)

[RFC3492](http://www.ietf.org/rfc/rfc3492.txt) (Punycode)

[addressable](http://github.com/sporkmonger/addressable) (Ruby URI implementation)

[punycode4r](http://raa.ruby-lang.org/project/punycode4r/) (Ruby punycode implementation)

[Unicode Character Database](http://www.unicode.org/Public/UNIDATA/UCD.html)

[UAX #15](http://www.unicode.org/reports/tr15/) (Unicode Normalization Forms)
