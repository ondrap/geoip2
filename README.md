GeoIP2 - library for accessing GeoIP2 database
==========

GeoIP2 is a haskell binding to the MaxMind GeoIP2 database.
It parses the database according to the MaxMind DB
[specificaiton](http://maxmind.github.io/MaxMind-DB),
version 2 of the specification is supported. The free geolite2 database can
be downloaded at http://dev.maxmind.com/geoip/geoip2/geolite2/.

See haddock documentaion for details.

```Haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.GeoIP2
import Data.IP (IP(..))

main = do
  geodb <- openGeoDB "GeoLite2-City.mmdb"
  let ip = IPv4 "23.253.242.70"
  print $ (findGeoData geodb "en" ip :: Maybe GeoResult)

  let ip2 = IPv6 "2001:4800:7817:104:be76:4eff:fe04:f608"
  print $ (findGeoData geodb "en" ip2 :: Maybe GeoResult)
```
