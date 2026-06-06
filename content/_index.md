---
title: Pickup Position DB
modules: ["simple-datatables"]
---

A modest searchable and sortable list of pickup measurements. Mostly compiled by the good people of [bassic.de](https://www.bassic.de/threads/pu-positions-database.14789156/).

The list below consists of:

 * __#__: the counter/id of the bass. Sort by this to group all measurements by bass
 * __Brand__: the brand of the bass
 * __Make__: the make of the bass
 * __Scale__: the scale of the bass. For historic and convention reasons given in a strange and irrational unit (inch)
 * __Pickup/Coil__: a short (!) textual description of the measurement. Sometimes just the name of the pickup (singlecoils) or also the name of the coil (in case of humbuckers or splitcoils). Sometimes multiple measurements per coil must be given (at an angle)
 * __Measurement__: the actual measurement. In a proper unit (cm; rounded to mm). Measured from the middle 12th fret to the middle of the respective pole piece. If the pole piece is not visible the edges of the pickup can be measured or an estimation can be made (add a __Comment__).
 * __Normalized__: the measurement scaled down to a virtual bass with a 1 inch scale (rounded to 4 decimal points). This makes measurements of basses with different scales comparable (and sortable). Multiply this value by the target scale (in inch) to get the position of the coil (in cm) measured from the 12th fret.
 * __Target *X*__: The measurement scaled to a bass of scale *X* (in cm, rounded to mm)
 * __Reporter__: Name or handle of the person or user who provided the measurement
 * __Comment__: An additional comment if the measurement needs any


This version deliberately only shows one target scale (for a clean interface).

See the {{< link "./multiscale" >}}multiscale version{{< /link >}} for more than one target scale.

This project is published under the GPL v3 license. This explicitly includes the data of the following table.

{{< table sortable="true" searchable="true" >}}
|  #  | Brand | Make | Scale | Pickup/Coil | Measurement | Normalized | Target 34″ | Reporter | Comment |
| --- | ----- | ---- | ----- | ----------- | ----------- | ---------- | ---------- | -------- | ------- |
| 1 | Fender | CIJ Jazzbass JB75-100NAT/R | 34.0″ | J: Neck | 27.5cm | 0.8088 | *27.5cm* | Doschd |  |
| 1 | Fender | CIJ Jazzbass JB75-100NAT/R | 34.0″ | J: Bridge | 36.5cm | 1.0735 | *36.5cm* | Doschd |  |
| 2 | Fender-like self-built | Jazz Bass | 34.0″ | J: Neck | 28.0cm | 0.8235 | *28.0cm* | BassManni |  |
| 2 | Fender-like self-built | Jazz Bass | 34.0″ | J: Bridge | 36.6cm | 1.0765 | *36.6cm* | BassManni |  |
| 3 | Fender | USA Jazz Standard S1 | 34.0″ | J: Neck | 27.7cm | 0.8147 | *27.7cm* | lenni |  |
| 3 | Fender | USA Jazz Standard S1 | 34.0″ | J: Bridge | 36.8cm | 1.0824 | *36.8cm* | lenni |  |
| 4 | Fender | CIJ Jazz Bass 62-83US | 34.0″ | J: Neck | 27.7cm | 0.8147 | *27.7cm* | griznak |  |
| 4 | Fender | CIJ Jazz Bass 62-83US | 34.0″ | J: Bridge | 36.8cm | 1.0824 | *36.8cm* | griznak |  |
| 5 | Fender | Jazzbass Fretless (1998) | 34.0″ | J: Neck | 27.7cm | 0.8147 | *27.7cm* | EMUBASS |  |
| 5 | Fender | Jazzbass Fretless (1998) | 34.0″ | J: Bridge | 36.8cm | 1.0824 | *36.8cm* | EMUBASS |  |
| 6 | Fender | Fender JB Special Edition 70s Spacing (1982?) | 34.0″ | J: Neck | 27.7cm | 0.8147 | *27.7cm* | BassManni |  |
| 6 | Fender | Fender JB Special Edition 70s Spacing (1982?) | 34.0″ | J: Bridge | 37.7cm | 1.1088 | *37.7cm* | BassManni |  |
| 7 | Johnson | Jazzbass | 34.0″ | J: Neck | 27.8cm | 0.8176 | *27.8cm* | BigB |  |
| 7 | Johnson | Jazzbass | 34.0″ | J: Bridge | 38.0cm | 1.1176 | *38.0cm* | BigB |  |
| 8 | Warwick | Streamer Stage 2 (2 J-Singlecoils) | 34.0″ | J: Neck | 31.4cm | 0.9235 | *31.4cm* | jam_bass |  |
| 8 | Warwick | Streamer Stage 2 (2 J-Singlecoils) | 34.0″ | J: Bridge | 38.5cm | 1.1324 | *38.5cm* | jam_bass |  |
| 9 | Fender | Jaguar Deluxe MIJ (2014) | 34.0″ | J: Neck | 27.4cm | 0.8059 | *27.4cm* | stoneface |  |
| 9 | Fender | Jaguar Deluxe MIJ (2014) | 34.0″ | J: Bridge | 38.9cm | 1.1441 | *38.9cm* | stoneface |  |
| 10 | Fenix | Jazzbass (1989) | 34.0″ | J: Neck | 27.6cm | 0.8118 | *27.6cm* | stoneface |  |
| 10 | Fenix | Jazzbass (1989) | 34.0″ | J: Bridge | 36.7cm | 1.0794 | *36.7cm* | stoneface |  |
| 11 | Guilber | ??? (1992) | 34.0″ | J: Neck | 26.9cm | 0.7912 | *26.9cm* | stoneface |  |
| 11 | Guilber | ??? (1992) | 34.0″ | J: Bridge | 36.5cm | 1.0735 | *36.5cm* | stoneface |  |
| 12 | Fender | Victor Bailey JB (2007) | 34.0″ | J: Neck | 27.5cm | 0.8088 | *27.5cm* | olebass |  |
| 12 | Fender | Victor Bailey JB (2007) | 34.0″ | J: Bridge | 36.5cm | 1.0735 | *36.5cm* | olebass |  |
| 13 | Lodestone | Primal Artist V JB | 34.0″ | J: Neck | 28.0cm | 0.8235 | *28.0cm* | olebass |  |
| 13 | Lodestone | Primal Artist V JB | 34.0″ | J: Bridge | 37.0cm | 1.0882 | *37.0cm* | olebass |  |
| 14 | Sandberg | California JM V (2005) | 34.0″ | J: Neck | 28.0cm | 0.8235 | *28.0cm* | olebass |  |
| 14 | Sandberg | California JM V (2005) | 34.0″ | J: Bridge | 36.9cm | 1.0853 | *36.9cm* | olebass |  |
| 15 | Vester | Clipper IV (1990) | 34.0″ | J: Neck | 30.0cm | 0.8824 | *30.0cm* | olebass |  |
| 15 | Vester | Clipper IV (1990) | 34.0″ | J: Bridge | 37.5cm | 1.1029 | *37.5cm* | olebass |  |
| 16 | Yamaha | BB Standard (1997) | 34.0″ | J: Neck | 27.2cm | 0.8000 | *27.2cm* | Youth |  |
| 16 | Yamaha | BB Standard (1997) | 34.0″ | J: Bridge | 36.4cm | 1.0706 | *36.4cm* | Youth |  |
| 17 | Yamaha | BB604 | 34.0″ | J: Neck | 30.5cm | 0.8971 | *30.5cm* | Youth |  |
| 17 | Yamaha | BB604 | 34.0″ | J: Bridge | 38.2cm | 1.1235 | *38.2cm* | Youth |  |
| 18 | Fender | Jaguar MIJ | 34.0″ | J: Neck | 27.3cm | 0.8029 | *27.3cm* | Youth |  |
| 18 | Fender | Jaguar MIJ | 34.0″ | J: Bridge | 37.7cm | 1.1088 | *37.7cm* | Youth |  |
| 19 | BolleBass | Corinna (2020) | 32.67″ | J: Neck | 27.9cm | 0.8540 | *29.0cm* | bassilisk |  |
| 19 | BolleBass | Corinna (2020) | 32.67″ | J: Bridge | 36.9cm | 1.1295 | *38.4cm* | bassilisk |  |
| 20 | Sandberg | TT (2013) | 34.0″ | J: Neck | 27.7cm | 0.8147 | *27.7cm* | Seven Basses |  |
| 20 | Sandberg | TT (2013) | 34.0″ | J: Bridge | 36.7cm | 1.0794 | *36.7cm* | Seven Basses |  |
| 21 | Marleaux | Votan X-Tra (2017) | 34.0″ | J: Neck | 27.5cm | 0.8088 | *27.5cm* | Seven Basses |  |
| 21 | Marleaux | Votan X-Tra (2017) | 34.0″ | J: Bridge | 36.5cm | 1.0735 | *36.5cm* | Seven Basses |  |
| 22 | Squier | Affinity Precision - 20th Anniversary | 34.0″ | P: E/A | 28.1cm | 0.8265 | *28.1cm* | Axel |  |
| 22 | Squier | Affinity Precision - 20th Anniversary | 34.0″ | P: D/G | 30.9cm | 0.9088 | *30.9cm* | Axel |  |
| 23 | Self-Built | Precision | 34.0″ | P: E/A | 28.1cm | 0.8265 | *28.1cm* | stoneface |  |
| 23 | Self-Built | Precision | 34.0″ | P: D/G | 30.9cm | 0.9088 | *30.9cm* | stoneface |  |
| 24 | Fender | Precision Road Worn (2011) | 34.0″ | P: E/A | 28.4cm | 0.8353 | *28.4cm* | indian66 |  |
| 24 | Fender | Precision Road Worn (2011) | 34.0″ | P: D/G | 31.1cm | 0.9147 | *31.1cm* | indian66 |  |
| 25 | Fender | Precision (1972) | 34.0″ | P: E/A | 28.3cm | 0.8324 | *28.3cm* | Nick McNoise |  |
| 25 | Fender | Precision (1972) | 34.0″ | P: D/G | 30.8cm | 0.9059 | *30.8cm* | Nick McNoise |  |
| 26 | Fender | 51 Model CIJ (Singlecoil) (2006) | 34.0″ | P Singlecoil | 26.2cm | 0.7706 | *26.2cm* |  |  |
| 27 | Aria | Pro II RSB-Deluxe (Revers P) (1983) | 34.0″ | P: E/A | 33.8cm | 0.9941 | *33.8cm* | Youth |  |
| 27 | Aria | Pro II RSB-Deluxe (Revers P) (1983) | 34.0″ | P: D/G | 32.1cm | 0.9441 | *32.1cm* | Youth |  |
| 28 | Fenix | Precision (1991) | 34.0″ | P: E/A | 28.3cm | 0.8324 | *28.3cm* | mosys |  |
| 28 | Fenix | Precision (1991) | 34.0″ | P: D/G | 31.0cm | 0.9118 | *31.0cm* | mosys |  |
| 29 | Travis Bean | TB 2000 (1977) | 33.25″ | SC: Neck | 18.9cm | 0.5684 | *19.3cm* | quarkfrosch |  |
| 29 | Travis Bean | TB 2000 (1977) | 33.25″ | SC: Bridge | 32.0cm | 0.9624 | *32.7cm* | quarkfrosch |  |
| 30 | Kramer | XKB-20 (1980) | 33.75″ | P: E/A | 27.4cm | 0.8119 | *27.6cm* | quarkfrosch |  |
| 30 | Kramer | XKB-20 (1980) | 33.75″ | P: D/G | 30.4cm | 0.9007 | *30.6cm* | quarkfrosch |  |
| 31 | Vantage | VP-795B (1979) | 33.25″ | P: Neck E/A | 22.3cm | 0.6707 | *22.8cm* | quarkfrosch |  |
| 31 | Vantage | VP-795B (1979) | 33.25″ | P: Neck D/G | 25.7cm | 0.7729 | *26.3cm* | quarkfrosch |  |
| 31 | Vantage | VP-795B (1979) | 33.25″ | P: Bridge E/A | 31.7cm | 0.9534 | *32.4cm* | quarkfrosch |  |
| 31 | Vantage | VP-795B (1979) | 33.25″ | P: Bridge D/G | 34.6cm | 1.0406 | *35.4cm* | quarkfrosch |  |
| 32 | Warwick | Corvette Std (2003) | 34.0″ | J: Neck | 29.8cm | 0.8765 | *29.8cm* | quarkfrosch |  |
| 32 | Warwick | Corvette Std (2003) | 34.0″ | 2J: Bridge Coil 1 | 38.0cm | 1.1176 | *38.0cm* | quarkfrosch |  |
| 32 | Warwick | Corvette Std (2003) | 34.0″ | 2J: Bridge Coil 2 | 40.4cm | 1.1882 | *40.4cm* | quarkfrosch |  |
| 33 | Warwick | Fortress One (1994) | 34.0″ | P: E/A | 30.2cm | 0.8882 | *30.2cm* | quarkfrosch |  |
| 33 | Warwick | Fortress One (1994) | 34.0″ | P: D/G | 33.0cm | 0.9706 | *33.0cm* | quarkfrosch |  |
| 33 | Warwick | Fortress One (1994) | 34.0″ | J: Bridge | 38.6cm | 1.1353 | *38.6cm* | quarkfrosch |  |
| 34 | Charvel | 2B (1988) | 34.0″ | P: E/A | 28.3cm | 0.8324 | *28.3cm* | quarkfrosch |  |
| 34 | Charvel | 2B (1988) | 34.0″ | P: D/G | 31.2cm | 0.9176 | *31.2cm* | quarkfrosch |  |
| 34 | Charvel | 2B (1988) | 34.0″ | J: Bridge | 37.7cm | 1.1088 | *37.7cm* | quarkfrosch |  |
| 35 | Fenix | P-Bass (1991) | 34.0″ | P: E/A | 28.2cm | 0.8294 | *28.2cm* | quarkfrosch |  |
| 35 | Fenix | P-Bass (1991) | 34.0″ | P: D/G | 31.1cm | 0.9147 | *31.1cm* | quarkfrosch |  |
| 35 | Fenix | P-Bass (1991) | 34.0″ | J: Bridge | 38.4cm | 1.1294 | *38.4cm* | quarkfrosch |  |
| 36 | Fenix | Jazz-Bass (1994) | 34.0″ | J: Neck | 27.5cm | 0.8088 | *27.5cm* | quarkfrosch |  |
| 36 | Fenix | Jazz-Bass (1994) | 34.0″ | J: Bridge | 36.7cm | 1.0794 | *36.7cm* | quarkfrosch |  |
| 37 | Fenix | Jazz-Bass V (1992) | 34.0″ | J: Neck | 27.5cm | 0.8088 | *27.5cm* | quarkfrosch |  |
| 37 | Fenix | Jazz-Bass V (1992) | 34.0″ | J: Bridge | 36.7cm | 1.0794 | *36.7cm* | quarkfrosch |  |
| 38 | Squier | Jaguar VM (2014) | 30.0″ | P: E/A | 22.8cm | 0.7600 | *25.8cm* | quarkfrosch |  |
| 38 | Squier | Jaguar VM (2014) | 30.0″ | P: D/G | 25.8cm | 0.8600 | *29.2cm* | quarkfrosch |  |
| 38 | Squier | Jaguar VM (2014) | 30.0″ | J: Bridge | 33.7cm | 1.1233 | *38.2cm* | quarkfrosch |  |
| 39 | Mayones | Slogan Classic 4 (2009) | 34.25″ | HB: Neck Coil 1 | 27.9cm | 0.8146 | *27.7cm* | quarkfrosch |  |
| 39 | Mayones | Slogan Classic 4 (2009) | 34.25″ | HB: Neck Coil 2 | 20.5cm | 0.5985 | *20.3cm* | quarkfrosch |  |
| 39 | Mayones | Slogan Classic 4 (2009) | 34.25″ | HB: Bridge Coil 1 | 36.7cm | 1.0715 | *36.4cm* | quarkfrosch |  |
| 39 | Mayones | Slogan Classic 4 (2009) | 34.25″ | HB: Bridge Coil 2 | 39.3cm | 1.1474 | *39.0cm* | quarkfrosch |  |
| 40 | OLP | Tony Levin 5 (2005) | 34.0″ | MM: Bridge Coil 1 | 33.9cm | 0.9971 | *33.9cm* | quarkfrosch |  |
| 40 | OLP | Tony Levin 5 (2005) | 34.0″ | MM: Bridge Coil 2 | 39.2cm | 1.1529 | *39.2cm* | quarkfrosch |  |
| 41 | Bass Collection | SB302 PWH (1987) | 34.25″ | P: E/A | 30.1cm | 0.8788 | *29.9cm* | quarkfrosch |  |
| 41 | Bass Collection | SB302 PWH (1987) | 34.25″ | P: D/G | 33.2cm | 0.9693 | *33.0cm* | quarkfrosch |  |
| 41 | Bass Collection | SB302 PWH (1987) | 34.25″ | J: Bridge | 38.1cm | 1.1124 | *37.8cm* | quarkfrosch |  |
| 42 | Wotan | Shark (1978) | 34.5″ | P: Neck E/A | 21.0cm | 0.6087 | *20.7cm* | quarkfrosch |  |
| 42 | Wotan | Shark (1978) | 34.5″ | P: Neck D/G | 23.7cm | 0.6870 | *23.4cm* | quarkfrosch |  |
| 42 | Wotan | Shark (1978) | 34.5″ | P: Bridge E/A | 34.2cm | 0.9913 | *33.7cm* | quarkfrosch |  |
| 42 | Wotan | Shark (1978) | 34.5″ | P: Bridge D/G | 37.7cm | 1.0928 | *37.2cm* | quarkfrosch |  |
| 43 | Kramer | DMZ 5000 (1980) | 33.75″ | P: Neck E/A | 27.5cm | 0.8148 | *27.7cm* | quarkfrosch |  |
| 43 | Kramer | DMZ 5000 (1980) | 33.75″ | P: Neck D/G | 30.3cm | 0.8978 | *30.5cm* | quarkfrosch |  |
| 43 | Kramer | DMZ 5000 (1980) | 33.75″ | J: Bridge | 39.0cm | 1.1556 | *39.3cm* | quarkfrosch |  |
| 44 | Esh | Stinger I | 34.25″ | HB: Neck Coil 1 | 31.1cm | 0.9080 | *30.9cm* | quarkfrosch |  |
| 44 | Esh | Stinger I | 34.25″ | HB: Neck Coil 1 | 33.1cm | 0.9664 | *32.9cm* | quarkfrosch |  |
| 44 | Esh | Stinger I | 34.25″ | HB: Bridge Coil 1 | 37.7cm | 1.1007 | *37.4cm* | quarkfrosch |  |
| 44 | Esh | Stinger I | 34.25″ | HB: Bridge Coil 2 | 39.7cm | 1.1591 | *39.4cm* | quarkfrosch |  |
| 45 | Esh | Stinger II - V | 34.25″ | HB: Neck Coil 1 | 29.0cm | 0.8467 | *28.8cm* | quarkfrosch |  |
| 45 | Esh | Stinger II - V | 34.25″ | HB: Neck Coil 2 | 31.0cm | 0.9051 | *30.8cm* | quarkfrosch |  |
| 45 | Esh | Stinger II - V | 34.25″ | HB: Bridge Coil 1 | 36.8cm | 1.0745 | *36.5cm* | quarkfrosch |  |
| 45 | Esh | Stinger II - V | 34.25″ | HB: Bridge Coil 2 | 38.8cm | 1.1328 | *38.5cm* | quarkfrosch |  |
| 46 | Daion | Power Mark X-B (1982) | 34.0″ | P: E/A | 30.2cm | 0.8882 | *30.2cm* | quarkfrosch |  |
| 46 | Daion | Power Mark X-B (1982) | 34.0″ | P: D/G | 33.0cm | 0.9706 | *33.0cm* | quarkfrosch |  |
| 47 | Gibson | Thunderbird (2005) | 34.0″ | Neck | 26.0cm | 0.7647 | *26.0cm* | Oli Wan | Only the middle of the humbucker was measured |
| 47 | Gibson | Thunderbird (2005) | 34.0″ | Bridge | 37.0cm | 1.0882 | *37.0cm* | Oli Wan | Only the middle of the humbucker was measured |
| 48 | Ibanez | TR Expressionist 500 | 34.0″ | Neck | 26.9cm | 0.7912 | *26.9cm* | lenni | Only the middle of the humbucker was measured |
| 48 | Ibanez | TR Expressionist 500 | 34.0″ | Bridge | 37.0cm | 1.0882 | *37.0cm* | lenni | Only the middle of the humbucker was measured |
| 49 | Ibanez | SR-500 | 34.0″ | Neck | 31.0cm | 0.9118 | *31.0cm* | Doschd | Only the middle of the humbucker was measured |
| 49 | Ibanez | SR-500 | 34.0″ | Bridge | 38.7cm | 1.1382 | *38.7cm* | Doschd | Only the middle of the humbucker was measured |
| 50 | Ibanez | BTB675 | 35.0″ | Neck | 29.1cm | 0.8314 | *28.3cm* | TheBrian | Only the middle of the humbucker was measured |
| 50 | Ibanez | BTB675 | 35.0″ | Bridge | 38.4cm | 1.0971 | *37.3cm* | TheBrian | Only the middle of the humbucker was measured |
| 51 | Lakland | Skyline 55-01 | 35.0″ | Neck | 27.6cm | 0.7886 | *26.8cm* | EMUBASS | Only the middle of the humbucker was measured. Bartolini Humbucker |
| 51 | Lakland | Skyline 55-01 | 35.0″ | Bridge | 35.9cm | 1.0257 | *34.9cm* | EMUBASS | Only the middle of the humbucker was measured. Bartolini Humbucker |
| 52 | Epiphone | Thunderbird Classic IV Pro (2015) | 34.0″ | Neck | 25.7cm | 0.7559 | *25.7cm* | stoneface | Only the middle of the humbucker was measured |
| 52 | Epiphone | Thunderbird Classic IV Pro (2015) | 34.0″ | Bridge | 36.7cm | 1.0794 | *36.7cm* | stoneface | Only the middle of the humbucker was measured |
| 53 | Esh | Sovereign | 34.0″ | Neck | 30.0cm | 0.8824 | *30.0cm* | Stratitis | Only the middle of the humbucker was measured. Soapbar, EMG35DC |
| 53 | Esh | Sovereign | 34.0″ | Bridge | 36.4cm | 1.0706 | *36.4cm* | Stratitis | Only the middle of the humbucker was measured. Soapbar, EMG35DC |
| 54 | Epiphone | Thunderbird Classic IV Pro (2018) | 34.0″ | Neck | 25.7cm | 0.7559 | *25.7cm* | indian66 | Only the middle of the humbucker was measured |
| 54 | Epiphone | Thunderbird Classic IV Pro (2018) | 34.0″ | Bridge | 36.7cm | 1.0794 | *36.7cm* | indian66 | Only the middle of the humbucker was measured |
| 55 | Esh | Sovereign V | 34.0″ | Neck | 31.8cm | 0.9353 | *31.8cm* | aBaxxi | Only the middle of the humbucker was measured. Soapbar. Probably BD5S |
| 55 | Esh | Sovereign V | 34.0″ | Bridge | 38.3cm | 1.1265 | *38.3cm* | aBaxxi | Only the middle of the humbucker was measured. Soapbar. Probably BD5S |
| 56 | Ibanez | RB850 (1986) | 34.0″ | Neck | 29.5cm | 0.8676 | *29.5cm* | boeb | Only the middle of the humbucker was measured |
| 56 | Ibanez | RB850 (1986) | 34.0″ | Bridge | 38.4cm | 1.1294 | *38.4cm* | boeb | Only the middle of the humbucker was measured |
| 57 | Washburn | Bantham XB400 | 34.0″ | Neck | 30.5cm | 0.8971 | *30.5cm* | Youth | Only the middle of the humbucker was measured |
| 57 | Washburn | Bantham XB400 | 34.0″ | Bridge | 37.5cm | 1.1029 | *37.5cm* | Youth | Only the middle of the humbucker was measured |
| 58 | von Paris | Piccolo (1999) | 30.0″ | Neck | 29.0cm | 0.9667 | *32.9cm* | Papa | Only the middle of the humbucker was measured |
| 58 | von Paris | Piccolo (1999) | 30.0″ | Bridge | 38.4cm | 1.2800 | *43.5cm* | Papa | Only the middle of the humbucker was measured |
| 59 | Yamaha | BBG4S II (2001) | 34.0″ | Neck | 29.7cm | 0.8735 | *29.7cm* | Seven Basses | Only the middle of the humbucker was measured |
| 59 | Yamaha | BBG4S II (2001) | 34.0″ | Bridge | 37.7cm | 1.1088 | *37.7cm* | Seven Basses | Only the middle of the humbucker was measured |
| 60 | Basscollection | SB301 | 34.0″ | P: Neck E/A | 29.5cm | 0.8676 | *29.5cm* | mulhofa |  |
| 60 | Basscollection | SB301 | 34.0″ | P: Neck D/G | 32.0cm | 0.9412 | *32.0cm* | mulhofa |  |
| 60 | Basscollection | SB301 | 34.0″ | J: Bridge | 37.5cm | 1.1029 | *37.5cm* | mulhofa |  |
| 61 | Squier Hohner | Frankenstein | 34.0″ | P: Neck E/A | 26.9cm | 0.7912 | *26.9cm* | EMUBASS |  |
| 61 | Squier Hohner | Frankenstein | 34.0″ | P: Neck D/G | 30.0cm | 0.8824 | *30.0cm* | EMUBASS |  |
| 61 | Squier Hohner | Frankenstein | 34.0″ | J: Bridge | 38.0cm | 1.1176 | *38.0cm* | EMUBASS |  |
| 62 | Warwick | Streamer LX-4 | 34.0″ | P: Neck E/A | 33.0cm | 0.9706 | *33.0cm* | Doschd |  |
| 62 | Warwick | Streamer LX-4 | 34.0″ | P: Neck D/G | 30.0cm | 0.8824 | *30.0cm* | Doschd |  |
| 62 | Warwick | Streamer LX-4 | 34.0″ | J: Bridge | 38.5cm | 1.1324 | *38.5cm* | Doschd |  |
| 63 | Warwick | Streamer One MEC (1990) | 34.0″ | P: Neck E/A | 33.2cm | 0.9765 | *33.2cm* | EMUBASS |  |
| 63 | Warwick | Streamer One MEC (1990) | 34.0″ | P: Neck D/G | 30.3cm | 0.8912 | *30.3cm* | EMUBASS |  |
| 63 | Warwick | Streamer One MEC (1990) | 34.0″ | J: Bridge | 38.7cm | 1.1382 | *38.7cm* | EMUBASS |  |
| 64 | Warwick | Fortress One MEC (1993) | 34.0″ | P: Neck E/A | 30.4cm | 0.8941 | *30.4cm* | EMUBASS |  |
| 64 | Warwick | Fortress One MEC (1993) | 34.0″ | P: Neck D/G | 33.3cm | 0.9794 | *33.3cm* | EMUBASS |  |
| 64 | Warwick | Fortress One MEC (1993) | 34.0″ | J: Bridge | 38.9cm | 1.1441 | *38.9cm* | EMUBASS |  |
| 65 | Warwick | Fortress One | 34.0″ | P: Neck E/A | 30.0cm | 0.8824 | *30.0cm* | Doschd |  |
| 65 | Warwick | Fortress One | 34.0″ | P: Neck D/G | 33.0cm | 0.9706 | *33.0cm* | Doschd |  |
| 65 | Warwick | Fortress One | 34.0″ | J: Bridge | 41.0cm | 1.2059 | *41.0cm* | Doschd | Measurment was marked as questionable in the old list |
| 66 | HotWire | Prezzman (2005) | 34.0″ | P: Neck E/A | 28.5cm | 0.8382 | *28.5cm* | olebass |  |
| 66 | HotWire | Prezzman (2005) | 34.0″ | P: Neck D/G | 31.5cm | 0.9265 | *31.5cm* | olebass |  |
| 66 | HotWire | Prezzman (2005) | 34.0″ | J: Bridge | 37.2cm | 1.0941 | *37.2cm* | olebass |  |
| 67 | Ibanez | Destroyer DT650 (1982) | 34.0″ | P: Neck E/A | 25.0cm | 0.7353 | *25.0cm* | boeb |  |
| 67 | Ibanez | Destroyer DT650 (1982) | 34.0″ | P: Neck D/G | 28.2cm | 0.8294 | *28.2cm* | boeb |  |
| 67 | Ibanez | Destroyer DT650 (1982) | 34.0″ | J: Bridge | 35.5cm | 1.0441 | *35.5cm* | boeb |  |
| 68 | Fender | Vintage Hot Rod 70s Jazz Bass | 34.0″ | P: Neck E/A | 25.8cm | 0.7588 | *25.8cm* | Xaver |  |
| 68 | Fender | Vintage Hot Rod 70s Jazz Bass | 34.0″ | P: Neck D/G | 28.5cm | 0.8382 | *28.5cm* | Xaver |  |
| 68 | Fender | Vintage Hot Rod 70s Jazz Bass | 34.0″ | J: Bridge | 38.0cm | 1.1176 | *38.0cm* | Xaver |  |
| 69 | Yamaha | BB414 | 34.0″ | P: Neck E/A | 27.8cm | 0.8176 | *27.8cm* | stoneface |  |
| 69 | Yamaha | BB414 | 34.0″ | P: Neck D/G | 30.9cm | 0.9088 | *30.9cm* | stoneface |  |
| 69 | Yamaha | BB414 | 34.0″ | J: Bridge | 37.5cm | 1.1029 | *37.5cm* | stoneface |  |
| 70 | Ibanez | Blazer | 34.0″ | P: Neck E/A | 25.7cm | 0.7559 | *25.7cm* | mosys |  |
| 70 | Ibanez | Blazer | 34.0″ | P: Neck D/G | 28.5cm | 0.8382 | *28.5cm* | mosys |  |
| 70 | Ibanez | Blazer | 34.0″ | J: Bridge | 35.9cm | 1.0559 | *35.9cm* | mosys |  |
| 71 | Yamaha | BB1024X (2012) | 34.0″ | P: Neck E/A | 28.0cm | 0.8235 | *28.0cm* | Seven Basses |  |
| 71 | Yamaha | BB1024X (2012) | 34.0″ | P: Neck D/G | 31.0cm | 0.9118 | *31.0cm* | Seven Basses |  |
| 71 | Yamaha | BB1024X (2012) | 34.0″ | J: Bridge | 37.5cm | 1.1029 | *37.5cm* | Seven Basses |  |
| 72 | Squier | Affinity P (2014) | 34.0″ | P: Neck E/A | 28.0cm | 0.8235 | *28.0cm* | Seven Basses | with EMG GZR |
| 72 | Squier | Affinity P (2014) | 34.0″ | P: Neck D/G | 31.0cm | 0.9118 | *31.0cm* | Seven Basses | with EMG GZR |
| 72 | Squier | Affinity P (2014) | 34.0″ | J: Bridge | 38.0cm | 1.1176 | *38.0cm* | Seven Basses | with EMG GZR |
| 73 | Dingwall | SP-1 5-string | 34.8″ | P: Neck-Coil. B-String  | 29.0cm | 0.8333 | *28.3cm* | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 34.8″ | J: Bridge. B-String | 36.0cm | 1.0345 | *35.2cm* | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 34.02″ | P: Neck-Coil. E-String | 30.5cm | 0.8965 | *30.5cm* | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 34.02″ | J: Bridge. E-String | 35.5cm | 1.0435 | *35.5cm* | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 33.39″ | P: Bridge-Coil. A-String | 30.0cm | 0.8985 | *30.5cm* | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 33.39″ | J: Bridge. A-String | 35.0cm | 1.0482 | *35.6cm* | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 32.68″ | P: Bridge-Coil. D-String | 30.6cm | 0.9364 | *31.8cm* | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 32.68″ | J: Bridge. D-String | 35.4cm | 1.0832 | *36.8cm* | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 31.89″ | P: Bridge-Coil. G-String | 29.0cm | 0.9094 | *30.9cm* | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 31.89″ | J: Bridge. G-String | 34.0cm | 1.0662 | *36.3cm* | foxtrott |  |
| 74 | Höfner | 500/1 | 30.0″ | Neck | 23.5cm | 0.7833 | *26.6cm* | Oli Wan |  |
| 74 | Höfner | 500/1 | 30.0″ | Bridge | 39.4cm | 1.3133 | *44.7cm* | Oli Wan |  |
| 75 | Rickenbacker | 4003/V63 (1991) | 33.25″ | Neck | 21.7cm | 0.6526 | *22.2cm* | Oli Wan |  |
| 75 | Rickenbacker | 4003/V63 (1991) | 33.25″ | Bridge | 32.7cm | 0.9835 | *33.4cm* | Oli Wan |  |
| 76 | Sandberg | California MM-5 | 35.0″ | Neck | 21.7cm | 0.6200 | *21.1cm* | Doschd | Only the middle of the humbucker was measured |
| 76 | Sandberg | California MM-5 | 35.0″ | Bridge | 32.7cm | 0.9343 | *31.8cm* | Doschd | Only the middle of the humbucker was measured |
| 77 | MusicMan | Sterling 4-String | 34.0″ | MM | 34.3cm | 1.0088 | *34.3cm* | Bassonovo | Only the middle of the humbucker was measured |
| 78 | Ibanez | ATK 300 | 34.0″ | Humbucker | 34.7cm | 1.0206 | *34.7cm* | griznak | Triplecoil with dummy-coil. Only the middle of the humbucker was measured |
| 79 | Warwick | Rockbass Corvette $$ | 34.0″ | Neck | 29.7cm | 0.8735 | *29.7cm* | EMUBASS | Not clear whether the model had soapbars or singlecoils. |
| 79 | Warwick | Rockbass Corvette $$ | 34.0″ | Bridge | 35.7cm | 1.0500 | *35.7cm* | EMUBASS | Not clear whether the model had soapbars or singlecoils. |
| 80 | Rickenbacker | 4003 (2010) | 33.25″ | Neck | 21.6cm | 0.6496 | *22.1cm* | stoneface |  |
| 80 | Rickenbacker | 4003 (2010) | 33.25″ | Bridge | 32.9cm | 0.9895 | *33.6cm* | stoneface |  |
| 81 | Danelectro | 63 Longscale | 34.0″ | Neck | 28.2cm | 0.8294 | *28.2cm* | Axel |  |
| 81 | Danelectro | 63 Longscale | 34.0″ | Bridge | 38.5cm | 1.1324 | *38.5cm* | Axel |  |
| 82 | Career | Stingray Copy | 34.0″ | MM | 34.8cm | 1.0235 | *34.8cm* | triple-U | Only the middle of the humbucker was measured |
| 83 | Esh | Notorious 2 V (2005) | 34.0″ | MM | 35.0cm | 1.0294 | *35.0cm* | olebass | Only the middle of the humbucker was measured |
| 84 | Rickenbacker | 4003S5 (2019) | 33.25″ | Neck | 21.7cm | 0.6526 | *22.2cm* | Mc Valve |  |
| 84 | Rickenbacker | 4003S5 (2019) | 33.25″ | Bridge | 35.3cm | 1.0617 | *36.1cm* | Mc Valve |  |
| 85 | Fender | Jazz Bass V - Roscoe Beck (1996) | 34.0″ | Neck PU; Neck Coil | 27.7cm | 0.8147 | *27.7cm* | Lowender | Double J Humbucker. Measurments were taken from a photo and might be off. |
| 85 | Fender | Jazz Bass V - Roscoe Beck (1996) | 34.0″ | Neck PU; Bridge Coil | 29.4cm | 0.8647 | *29.4cm* | Lowender | Double J Humbucker. Measurments were taken from a photo and might be off. |
| 85 | Fender | Jazz Bass V - Roscoe Beck (1996) | 34.0″ | Bridge PU; Neck Coil | 36.7cm | 1.0794 | *36.7cm* | Lowender | Double J Humbucker. Measurments were taken from a photo and might be off. |
| 85 | Fender | Jazz Bass V - Roscoe Beck (1996) | 34.0″ | Bridge PU; Bridge Coil | 38.4cm | 1.1294 | *38.4cm* | Lowender | Double J Humbucker. Measurments were taken from a photo and might be off. |
| 86 | MusicMan | Stingray 4 HH | 34.0″ | Neck PU; Neck Coil | 25.3cm | 0.7441 | *25.3cm* | Marten |  |
| 86 | MusicMan | Stingray 4 HH | 34.0″ | Neck PU; Bridge Coil | 27.7cm | 0.8147 | *27.7cm* | Marten |  |
| 86 | MusicMan | Stingray 4 HH | 34.0″ | Bridge PU; Neck Coil | 34.4cm | 1.0118 | *34.4cm* | Marten |  |
| 86 | MusicMan | Stingray 4 HH | 34.0″ | Bridge PU; Bridge Coil | 36.8cm | 1.0824 | *36.8cm* | Marten |  |
| 87 | MusicMan | Stingray 5 H (2009) | 34.0″ | MM Neck Coil | 33.1cm | 0.9735 | *33.1cm* | 4enima |  |
| 87 | MusicMan | Stingray 5 H (2009) | 34.0″ | MM Bridge Coil | 35.4cm | 1.0412 | *35.4cm* | 4enima |  |
| 88 | Harley Benton | Marquess-4 | 34.0″ | P: E/A | 29.5cm | 0.8676 | *29.5cm* | Dommbasstisch |  |
| 88 | Harley Benton | Marquess-4 | 34.0″ | P: D/G | 32.3cm | 0.9500 | *32.3cm* | Dommbasstisch |  |
| 88 | Harley Benton | Marquess-4 | 34.0″ | J: Bridge | 38.6cm | 1.1353 | *38.6cm* | Dommbasstisch |  |
| 89 | Harley Benton | PJ-4 SBK | 34.0″ | P: E/A | 28.5cm | 0.8382 | *28.5cm* | Dommbasstisch |  |
| 89 | Harley Benton | PJ-4 SBK | 34.0″ | P: D/G | 31.3cm | 0.9206 | *31.3cm* | Dommbasstisch |  |
| 89 | Harley Benton | PJ-4 SBK | 34.0″ | J: Bridge | 38.5cm | 1.1324 | *38.5cm* | Dommbasstisch |  |
| 90 | Yamaha | RBX374 | 34.0″ | HB: Neck Coil 1 | 26.9cm | 0.7912 | *26.9cm* | Dommbasstisch |  |
| 90 | Yamaha | RBX374 | 34.0″ | HB: Neck Coil 2 | 28.7cm | 0.8441 | *28.7cm* | Dommbasstisch |  |
| 90 | Yamaha | RBX374 | 34.0″ | HB: Bridge Coil 1 | 34.9cm | 1.0265 | *34.9cm* | Dommbasstisch |  |
| 90 | Yamaha | RBX374 | 34.0″ | HB: Bridge Coil 2 | 36.5cm | 1.0735 | *36.5cm* | Dommbasstisch |  |
| 91 | Ibanez | BTB200 | 35.0″ | HB: Neck Coil 1 | 26.7cm | 0.7629 | *25.9cm* | Dommbasstisch |  |
| 91 | Ibanez | BTB200 | 35.0″ | HB: Neck Coil 2 | 29.5cm | 0.8429 | *28.7cm* | Dommbasstisch |  |
| 91 | Ibanez | BTB200 | 35.0″ | HB: Bridge Coil 1 | 35.8cm | 1.0229 | *34.8cm* | Dommbasstisch |  |
| 91 | Ibanez | BTB200 | 35.0″ | HB: Bridge Coil 2 | 38.6cm | 1.1029 | *37.5cm* | Dommbasstisch |  |
| 92 | Squier | Vintage Modified Precision Bass TB | 34.0″ | HB: E/A | 19.4cm | 0.5706 | *19.4cm* |  | Apparently a split-coil in a fat humbucker housing. |
| 92 | Squier | Vintage Modified Precision Bass TB | 34.0″ | HB: D/G | 21.8cm | 0.6412 | *21.8cm* |  | Apparently a split-coil in a fat humbucker housing. |
| 93 | G&L | SB-1 (1982) | 34.0″ | Singlecoil (Splitcoil?) | 29.4cm | 0.8647 | *29.4cm* | matteagle |  |
| 94 | Guild | Newark St. Starfire I (2013) | 30.75″ | Singlecoil | 31.1cm | 1.0114 | *34.4cm* |  | Bisonic Single-Coil |
| 95 | Fender | Bullet B30 | 30.0″ | Splitcoil: E/A | 26.5cm | 0.8833 | *30.0cm* | matt-o- |  |
| 95 | Fender | Bullet B30 | 30.0″ | Splitcoil: D/G | 28.5cm | 0.9500 | *32.3cm* | matt-o- |  |
| 96 | Squier | Bronco | 30.0″ | Singlecoil | 23.9cm | 0.7967 | *27.1cm* | matt-o- |  |
| 97 | Epiphone | Jack Casady | 34.0″ | Middle of humbucker | 29.0cm | 0.8529 | *29.0cm* | DaniG3 | Polepieces not visible. Breadth of pickup: 3cm |
| 98 | Modulus | Flea | 34.0″ | Middle of humbucker | 33.8cm | 0.9941 | *33.8cm* | DaniG3 | Polepieces not visible. Breadth of pickup: 4.8cm |
| 99 | Yamaha | Attitude 3 | 34.0″ | Neck HB; Coil 1 | 21.6cm | 0.6353 | *21.6cm* | DaniG3 |  |
| 99 | Yamaha | Attitude 3 | 34.0″ | Neck HB; Coil 2 | 23.3cm | 0.6853 | *23.3cm* | DaniG3 |  |
| 99 | Yamaha | Attitude 3 | 34.0″ | P: E/A | 28.3cm | 0.8324 | *28.3cm* | DaniG3 |  |
| 99 | Yamaha | Attitude 3 | 34.0″ | P: D/G | 31.0cm | 0.9118 | *31.0cm* | DaniG3 |  |
| 100 | Warwick | Streamer (1984) | 34.0″ | P: E/A | 32.7cm | 0.9618 | *32.7cm* | xsteps | reverse P |
| 100 | Warwick | Streamer (1984) | 34.0″ | P: D/G | 29.9cm | 0.8794 | *29.9cm* | xsteps | reverse P |
| 100 | Warwick | Streamer (1984) | 34.0″ | J: Bridge | 38.3cm | 1.1265 | *38.3cm* | xsteps |  |
| 101 | Gibson | Thunderbird (1996) | 34.0″ | HB: Neck | 25.9cm | 0.7618 | *25.9cm* | MauMau | Only the middle of the humbucker was measured |
| 101 | Gibson | Thunderbird (1996) | 34.0″ | HB: Bridge | 36.9cm | 1.0853 | *36.9cm* | MauMau | Only the middle of the humbucker was measured |
| 102 | Spector | Euro LX5 | 35.0″ | HB: Neck | 31.4cm | 0.8971 | *30.5cm* | MauMau | Only the middle of the humbucker was measured |
| 102 | Spector | Euro LX5 | 35.0″ | HB: Bridge | 38.3cm | 1.0943 | *37.2cm* | MauMau | Only the middle of the humbucker was measured |
| 103 | Self-Built | Leland Sklar Frankenstein | 34.0″ | Neck: P E/A | 24.7cm | 0.7265 | *24.7cm* | KTi01 | measurements were revers engineered from various pictures. reverse P |
| 103 | Self-Built | Leland Sklar Frankenstein | 34.0″ | Neck: P D/G | 21.7cm | 0.6382 | *21.7cm* | KTi01 | measurements were revers engineered from various pictures. reverse P |
| 103 | Self-Built | Leland Sklar Frankenstein | 34.0″ | Bridge: P E/A | 36.9cm | 1.0853 | *36.9cm* | KTi01 | measurements were revers engineered from various pictures. reverse P |
| 103 | Self-Built | Leland Sklar Frankenstein | 34.0″ | Bridge: P D/G | 34.1cm | 1.0029 | *34.1cm* | KTi01 | measurements were revers engineered from various pictures. reverse P |
| 104 | Danelectro | Longhorn | 29.75″ | Neck @ G-String | 24.3cm | 0.8168 | *27.8cm* | Mad Jazz Morales | the pickups are installed at an angle |
| 104 | Danelectro | Longhorn | 29.75″ | Neck @ E-String | 23.8cm | 0.8000 | *27.2cm* | Mad Jazz Morales | the pickups are installed at an angle |
| 104 | Danelectro | Longhorn | 29.75″ | Bridge @ G-String | 34.3cm | 1.1529 | *39.2cm* | Mad Jazz Morales | the pickups are installed at an angle |
| 104 | Danelectro | Longhorn | 29.75″ | Bridge @ E-String | 33.8cm | 1.1361 | *38.6cm* | Mad Jazz Morales | the pickups are installed at an angle |
| 105 | Sire | U5 Shortscale | 30.0″ | P: E/A | 23.2cm | 0.7733 | *26.3cm* | triple-U |  |
| 105 | Sire | U5 Shortscale | 30.0″ | P: D/G | 25.9cm | 0.8633 | *29.4cm* | triple-U |  |
| 105 | Sire | U5 Shortscale | 30.0″ | J: Bridge | 34.7cm | 1.1567 | *39.3cm* | triple-U |  |
| 106 | Sadowsky | MetroExpress Jazz Bass 21 Vintage (Gen. 1) | 34.0″ | J: Neck | 28.5cm | 0.8382 | *28.5cm* | xsteps |  |
| 106 | Sadowsky | MetroExpress Jazz Bass 21 Vintage (Gen. 1) | 34.0″ | J: Bridge | 37.5cm | 1.1029 | *37.5cm* | xsteps |  |
| 107 | MusicMan | Stingray SS4 Shortscale | 30.0″ | MM | 303.0cm | 10.1000 | *343.4cm* | triple-U | Only the middle of the humbucker was measured |
| 108 | Fender | Cabronita Precision | 34.0″ | Splitcoil: E/A | 30.9cm | 0.9088 | *30.9cm* | basslife | Replacement Pickup: TV Jones Thundermag |
| 108 | Fender | Cabronita Precision | 34.0″ | Splitcoil: D/G | 29.2cm | 0.8588 | *29.2cm* | basslife | Replacement Pickup: TV Jones Thundermag |
| 109 | Rickenbacker | 4003S5 Fireglo | 33.25″ | Neck | 21.2cm | 0.6376 | *21.7cm* | McValve |  |
| 109 | Rickenbacker | 4003S5 Fireglo | 33.25″ | Bridge | 34.5cm | 1.0376 | *35.3cm* | McValve |  |
| 110 | Hohner | B2 (1989) | 34.0″ | Neck | 28.2cm | 0.8294 | *28.2cm* | Scriptura | Only the middle of the humbucker was measured |
| 110 | Hohner | B2 (1989) | 34.0″ | Bridge | 38.6cm | 1.1353 | *38.6cm* | Scriptura | Only the middle of the humbucker was measured |
| 111 | MusicMan | Stingray 3EQ Fretless (1991) | 34.0″ | MM | 34.5cm | 1.0147 | *34.5cm* | Scriptura | Only the middle of the humbucker was measured |
| 112 | Human Base | Jonas (2004) | 34.0″ | HB Neck | 29.5cm | 0.8676 | *29.5cm* | Scriptura | Only the middle of the humbucker was measured |
| 112 | Human Base | Jonas (2004) | 34.0″ | HB Bridge | 38.0cm | 1.1176 | *38.0cm* | Scriptura | Only the middle of the humbucker was measured |
| 113 | Warwick | RB Starbass | 32.0″ | J: Neck | 23.6cm | 0.7375 | *25.1cm* | triple-U |  |
| 113 | Warwick | RB Starbass | 32.0″ | J: Bridge | 34.6cm | 1.0812 | *36.8cm* | triple-U |  |
| 114 | Ibanez | Musician MC924 (1982) | 34.0″ | P: E/A | 26.9cm | 0.7912 | *26.9cm* | stoneface |  |
| 114 | Ibanez | Musician MC924 (1982) | 34.0″ | P: D/G | 29.9cm | 0.8794 | *29.9cm* | stoneface |  |
| 114 | Ibanez | Musician MC924 (1982) | 34.0″ | J: Bridge | 38.7cm | 1.1382 | *38.7cm* | stoneface |  |
| 115 | Johnson | Stingray Copy | 34.0″ | HB, Coil 1 | 34.0cm | 1.0000 | *34.0cm* | Axel | the humbucker is thinner than a MM pickup |
| 115 | Johnson | Stingray Copy | 34.0″ | HB, Coil 2 | 35.7cm | 1.0500 | *35.7cm* | Axel | the humbucker is thinner than a MM pickup |
| 116 | Hartwood | Satelite (2025) | 32.0″ | P: E/A | 20.2cm | 0.6312 | *21.5cm* | wasabi 2.0 |  |
| 116 | Hartwood | Satelite (2025) | 32.0″ | P: D/G | 23.0cm | 0.7188 | *24.4cm* | wasabi 2.0 |  |
| 117 | MusicMan | Stingray 5 H (1988, 1998, 1999, 2014) | 34.0″ | Coil 1 | 33.3cm | 0.9794 | *33.3cm* | 4enima |  |
| 117 | MusicMan | Stingray 5 H (1988, 1998, 1999, 2014) | 34.0″ | Coil 2 | 35.7cm | 1.0500 | *35.7cm* | 4enima |  |
| 118 | MusicMan | Big Al 5 SSS (2010) | 34.0″ | Neck | 24.1cm | 0.7088 | *24.1cm* | 4enima | Neodymium Single Coils |
| 118 | MusicMan | Big Al 5 SSS (2010) | 34.0″ | Middle | 30.8cm | 0.9059 | *30.8cm* | 4enima | Neodymium Single Coils |
| 118 | MusicMan | Big Al 5 SSS (2010) | 34.0″ | Bridge | 38.5cm | 1.1324 | *38.5cm* | 4enima | Neodymium Single Coils. installed at an angle. Only the value of the G string was given |
| 119 | MusicMan | Reflex 5 HSS (2012) | 34.0″ | Neck | 24.1cm | 0.7088 | *24.1cm* | 4enima | Neodymium Single Coil |
| 119 | MusicMan | Reflex 5 HSS (2012) | 34.0″ | Middle | 30.3cm | 0.8912 | *30.3cm* | 4enima | Neodymium Single Coil |
| 119 | MusicMan | Reflex 5 HSS (2012) | 34.0″ | Neck, Coil 1 | 36.3cm | 1.0676 | *36.3cm* | 4enima | Power Keramik Humbucker |
| 119 | MusicMan | Reflex 5 HSS (2012) | 34.0″ | Neck, Coil 2 | 38.6cm | 1.1353 | *38.6cm* | 4enima | Power Keramik Humbucker |
| 120 | MusicMan (Sterling by) | Stingray Shortscale | 30.0″ | Coil 1 | 29.2cm | 0.9733 | *33.1cm* | 4enima |  |
| 120 | MusicMan (Sterling by) | Stingray Shortscale | 30.0″ | Coil 2 | 31.6cm | 1.0533 | *35.8cm* | 4enima |  |
| 121 | Warwick | Thumb NT 5 | 34.0″ | Neck @ G String | 34.5cm | 1.0147 | *34.5cm* | 4enima | EMG SingleCoils at an angle |
| 121 | Warwick | Thumb NT 5 | 34.0″ | Neck @ B String | 35.9cm | 1.0559 | *35.9cm* | 4enima | EMG SingleCoils at an angle |
| 121 | Warwick | Thumb NT 5 | 34.0″ | Bridge @ G String | 38.5cm | 1.1324 | *38.5cm* | 4enima | EMG SingleCoils at an angle |
| 121 | Warwick | Thumb NT 5 | 34.0″ | Bridge @ B String | 39.9cm | 1.1735 | *39.9cm* | 4enima | EMG SingleCoils at an angle |
| 122 | Warwick | FNA Jazzman 5 / Streamer Jazzman 5 | 34.0″ | Neck @ G String | 28.9cm | 0.8500 | *28.9cm* | 4enima |  |
| 122 | Warwick | FNA Jazzman 5 / Streamer Jazzman 5 | 34.0″ | Neck @ B String | 29.8cm | 0.8765 | *29.8cm* | 4enima |  |
| 122 | Warwick | FNA Jazzman 5 / Streamer Jazzman 5 | 34.0″ | Bridge, Coil 1 | 34.7cm | 1.0206 | *34.7cm* | 4enima |  |
| 122 | Warwick | FNA Jazzman 5 / Streamer Jazzman 5 | 34.0″ | Bridge, Coil 2 | 37.6cm | 1.1059 | *37.6cm* | 4enima |  |
| 123 | Spector | Euro 5 (Limited White Edition) | 35.0″ | Neck, Coil 1 | 30.5cm | 0.8714 | *29.6cm* | 4enima |  |
| 123 | Spector | Euro 5 (Limited White Edition) | 35.0″ | Neck, Coil 2 | 32.5cm | 0.9286 | *31.6cm* | 4enima |  |
| 123 | Spector | Euro 5 (Limited White Edition) | 35.0″ | Bridge, Coil 1 | 37.5cm | 1.0714 | *36.4cm* | 4enima |  |
| 123 | Spector | Euro 5 (Limited White Edition) | 35.0″ | Bridge, Coil 2 | 39.5cm | 1.1286 | *38.4cm* | 4enima |  |
| 124 | Ibanez | EHB1506MS (multiscale) (2021) | 32.5″ | Neck @ C String | 29.0cm | 0.8923 | *30.3cm* | 4enima |  |
| 124 | Ibanez | EHB1506MS (multiscale) (2021) | 35.0″ | Neck @ B String | 31.2cm | 0.8914 | *30.3cm* | 4enima |  |
| 124 | Ibanez | EHB1506MS (multiscale) (2021) | 32.5″ | Bridge @ C String | 37.3cm | 1.1477 | *39.0cm* | 4enima |  |
| 124 | Ibanez | EHB1506MS (multiscale) (2021) | 35.0″ | Bridge @ B String | 39.4cm | 1.1257 | *38.3cm* | 4enima |  |
| 125 | Cort | SP-PB Precision Bass (2003) | 34.0″ | P: E/A | 28.1cm | 0.8265 | *28.1cm* | 4enima |  |
| 125 | Cort | SP-PB Precision Bass (2003) | 34.0″ | P: D/G | 30.8cm | 0.9059 | *30.8cm* | 4enima |  |
| 126 | Spear | S1-SP | 34.0″ | Neck, Coil 1 | 30.0cm | 0.8824 | *30.0cm* | 4enima |  |
| 126 | Spear | S1-SP | 34.0″ | Neck, Coil 2 | 31.5cm | 0.9265 | *31.5cm* | 4enima |  |
| 126 | Spear | S1-SP | 34.0″ | Bridge, Coil 1 | 37.9cm | 1.1147 | *37.9cm* | 4enima |  |
| 126 | Spear | S1-SP | 34.0″ | Bridge, Coil 2 | 39.4cm | 1.1588 | *39.4cm* | 4enima |  |
| 127 | Fligh | Mini Bass | 23.0″ | P: E/A | 20.7cm | 0.9000 | *30.6cm* | StWu |  |
| 127 | Fligh | Mini Bass | 23.0″ | P: D/G | 23.7cm | 1.0304 | *35.0cm* | StWu |  |
| 128 | Le Fay | Herr Schwarz 5 | 33.78″ | Neck @ B-String | 28.0cm | 0.8289 | *28.2cm* | hugee | Pickups are at a slight angle |
| 128 | Le Fay | Herr Schwarz 5 | 33.78″ | Neck @ G-String | 27.6cm | 0.8171 | *27.8cm* | hugee | Pickups are at a slight angle |
| 128 | Le Fay | Herr Schwarz 5 | 33.78″ | Middle @ B-String | 34.9cm | 1.0332 | *35.1cm* | hugee | Pickups are at a slight angle |
| 128 | Le Fay | Herr Schwarz 5 | 33.78″ | Middle @ G-String | 34.5cm | 1.0213 | *34.7cm* | hugee | Pickups are at a slight angle |
| 128 | Le Fay | Herr Schwarz 5 | 33.78″ | Bridge @ B-String | 38.1cm | 1.1279 | *38.3cm* | hugee | Pickups are at a slight angle |
| 128 | Le Fay | Herr Schwarz 5 | 33.78″ | Bridge @ G-String | 37.7cm | 1.1160 | *37.9cm* | hugee | Pickups are at a slight angle |
| 129 | Fender | Dimension 5 USA Deluxe | 34.0″ | Neck, Coil 1 | 26.9cm | 0.7912 | *26.9cm* | Reislöffel |  |
| 129 | Fender | Dimension 5 USA Deluxe | 34.0″ | Neck, Coil 2 | 28.7cm | 0.8441 | *28.7cm* | Reislöffel |  |
| 129 | Fender | Dimension 5 USA Deluxe | 34.0″ | Bridge, Coil 1 | 36.4cm | 1.0706 | *36.4cm* | Reislöffel |  |
| 129 | Fender | Dimension 5 USA Deluxe | 34.0″ | Bridge, Coil 2 | 38.2cm | 1.1235 | *38.2cm* | Reislöffel |  |
| 130 | BassCulture | BoltOn (1990) | 33.8″ | Neck, Coil 1 | 28.6cm | 0.8462 | *28.8cm* | Jazz62 |  |
| 130 | BassCulture | BoltOn (1990) | 33.8″ | Neck, Coil 2 | 30.6cm | 0.9053 | *30.8cm* | Jazz62 |  |
| 130 | BassCulture | BoltOn (1990) | 33.8″ | Bridge, Coil 1 | 35.6cm | 1.0533 | *35.8cm* | Jazz62 |  |
| 130 | BassCulture | BoltOn (1990) | 33.8″ | Bridge, Coil 2 | 37.6cm | 1.1124 | *37.8cm* | Jazz62 |  |
| 131 | Guild | JetStar 1 (1974) | 30.7″ | Bisonic | 19.8cm | 0.6450 | *21.9cm* | Jazz62 | Only neck-sided coil is magnetic |
| 132 | Schack | Carbon Headless | 34.0″ | Neck | 28.5cm | 0.8382 | *28.5cm* | Stefano | Only the middle of the pickups was measured. The pickups are 36mm wide |
| 132 | Schack | Carbon Headless | 34.0″ | Bridge | 39.0cm | 1.1471 | *39.0cm* | Stefano | Only the middle of the pickups was measured. The pickups are 36mm wide |
| 133 | Guyatone | EB-1 (1967) | 24.75″ | Middle @ E-String | 15.4cm | 0.6222 | *21.2cm* | BootsyCollins |  |
| 133 | Guyatone | EB-1 (1967) | 24.75″ | Middle @ G-String | 15.1cm | 0.6101 | *20.7cm* | BootsyCollins |  |
| 134 | Sandberg | Ken Taylor | 34.2″ | Neck, Coil 1 | 29.7cm | 0.8684 | *29.5cm* | Bassionator |  |
| 134 | Sandberg | Ken Taylor | 34.2″ | Neck, Coil 2 | 32.0cm | 0.9357 | *31.8cm* | Bassionator |  |
| 134 | Sandberg | Ken Taylor | 34.2″ | Bridge, Coil 1 | 36.7cm | 1.0731 | *36.5cm* | Bassionator |  |
| 134 | Sandberg | Ken Taylor | 34.2″ | Bridge, Coil 2 | 39.1cm | 1.1433 | *38.9cm* | Bassionator |  |
| 135 | Squier | VI | 30.0″ | Neck | 19.8cm | 0.6600 | *22.4cm* | walktheline |  |
| 135 | Squier | VI | 30.0″ | Middle | 27.1cm | 0.9033 | *30.7cm* | walktheline |  |
| 135 | Squier | VI | 30.0″ | Bridge | 34.3cm | 1.1433 | *38.9cm* | walktheline |  |
| 136 | Fender | Precision Bass Special "Cowpoke" | 34.0″ | P: E/A | 28.0cm | 0.8235 | *28.0cm* | walktheline |  |
| 136 | Fender | Precision Bass Special "Cowpoke" | 34.0″ | P: D/G | 30.7cm | 0.9029 | *30.7cm* | walktheline |  |
| 136 | Fender | Precision Bass Special "Cowpoke" | 34.0″ | J: Bridge | 36.7cm | 1.0794 | *36.7cm* | walktheline |  |
| 137 | U-Bass (licensed by LeDuc) | ??? (Frettless with JJ Pickups) | 34.0″ | Neck @ E-String | 28.8cm | 0.8471 | *28.8cm* | Manuel (not bassic) |  |
| 137 | U-Bass (licensed by LeDuc) | ??? (Frettless with JJ Pickups) | 34.0″ | Neck @ G-String | 28.0cm | 0.8235 | *28.0cm* | Manuel (not bassic) |  |
| 137 | U-Bass (licensed by LeDuc) | ??? (Frettless with JJ Pickups) | 34.0″ | Bridge @ E-String | 37.4cm | 1.1000 | *37.4cm* | Manuel (not bassic) |  |
| 137 | U-Bass (licensed by LeDuc) | ??? (Frettless with JJ Pickups) | 34.0″ | Bridge @ G-String | 36.6cm | 1.0765 | *36.6cm* | Manuel (not bassic) |  |
| 138 | Gretsch | Junior Jet (1st Version with one pickup) | 29.8″ | middle | 24.0cm | 0.8054 | *27.4cm* | Manuel (not bassic) | only middle of pickup was measured. Width: 3.8cm |
| 139 | Washburn | Status S1000 LE | 34.0″ | J: Neck | 29.5cm | 0.8676 | *29.5cm* | evert | Equivalent to an 80s Status Energy |
| 139 | Washburn | Status S1000 LE | 34.0″ | J: Bridge | 39.0cm | 1.1471 | *39.0cm* | evert | Equivalent to an 80s Status Energy |
| 140 | Mr. Bassman | Slapper (Series 6) | 32.0″ | Neck, Coil 1 | 27.7cm | 0.8656 | *29.4cm* | Tiefton |  |
| 140 | Mr. Bassman | Slapper (Series 6) | 32.0″ | Neck, Coil 2 | 30.0cm | 0.9375 | *31.9cm* | Tiefton |  |
| 140 | Mr. Bassman | Slapper (Series 6) | 32.0″ | Bridge, Coil 1 | 38.0cm | 1.1875 | *40.4cm* | Tiefton |  |
| 140 | Mr. Bassman | Slapper (Series 6) | 32.0″ | Bridge, Coil 2 | 40.3cm | 1.2594 | *42.8cm* | Tiefton |  |
| 141 | Chowny | Retrovibe Vantage | 33.8″ | Neck, Coil 1 | 22.7cm | 0.6716 | *22.8cm* | EMUBASS |  |
| 141 | Chowny | Retrovibe Vantage | 33.8″ | Neck, Coil 2 | 25.9cm | 0.7663 | *26.1cm* | EMUBASS |  |
| 141 | Chowny | Retrovibe Vantage | 33.8″ | Bridge, Coil 1 | 32.6cm | 0.9645 | *32.8cm* | EMUBASS |  |
| 141 | Chowny | Retrovibe Vantage | 33.8″ | Bridge, Coil 2 | 35.0cm | 1.0355 | *35.2cm* | EMUBASS |  |
| 142 | Burns | Marquee | 31.9″ | Neck @ E-String | 19.2cm | 0.6019 | *20.5cm* | PeterChristof | should equal a 1964 Vista Sonic |
| 142 | Burns | Marquee | 31.9″ | Neck @ G-String | 20.0cm | 0.6270 | *21.3cm* | PeterChristof | should equal a 1964 Vista Sonic |
| 142 | Burns | Marquee | 31.9″ | Middle @ E-String | 26.5cm | 0.8307 | *28.2cm* | PeterChristof | should equal a 1964 Vista Sonic |
| 142 | Burns | Marquee | 31.9″ | Middle @ G-String | 27.3cm | 0.8558 | *29.1cm* | PeterChristof | should equal a 1964 Vista Sonic |
| 142 | Burns | Marquee | 31.9″ | Bridge @ E-String | 33.6cm | 1.0533 | *35.8cm* | PeterChristof | should equal a 1964 Vista Sonic |
| 142 | Burns | Marquee | 31.9″ | Bridge @ G-String | 34.3cm | 1.0752 | *36.6cm* | PeterChristof | should equal a 1964 Vista Sonic |
| 143 | Ibanez | 2354 B (1974/75) | 30.3″ | Neck | 18.4cm | 0.6073 | *20.6cm* | krausinger | Mudbucker. Only one set of polepieces. Breadth: 77mm |
| 143 | Ibanez | 2354 B (1974/75) | 30.3″ | Bridge, Coil 1 | 32.3cm | 1.0660 | *36.2cm* | krausinger |  |
| 143 | Ibanez | 2354 B (1974/75) | 30.3″ | Bridge, Coil 2 | 34.5cm | 1.1386 | *38.7cm* | krausinger |  |
| 144 | Ibanez | 2354 LB (1974/75) | 30.3″ | Neck | 18.5cm | 0.6106 | *20.8cm* | krausinger | Mudbucker. Only one set of polepieces. Breadth: 77mm |
| 144 | Ibanez | 2354 LB (1974/75) | 30.3″ | Bridge, Coil 1 | 32.3cm | 1.0660 | *36.2cm* | krausinger |  |
| 144 | Ibanez | 2354 LB (1974/75) | 30.3″ | Bridge, Coil 2 | 34.6cm | 1.1419 | *38.8cm* | krausinger |  |
| 145 | Ibanez | 2354 NB (1974/75) | 33.5″ | Neck | 20.2cm | 0.6030 | *20.5cm* | krausinger | Mudbucker. Only one set of polepieces. Breadth: 77mm |
| 145 | Ibanez | 2354 NB (1974/75) | 33.5″ | Bridge, Coil 1 | 34.2cm | 1.0209 | *34.7cm* | krausinger |  |
| 145 | Ibanez | 2354 NB (1974/75) | 33.5″ | Bridge, Coil 2 | 36.4cm | 1.0866 | *36.9cm* | krausinger |  |
| 146 | Vantage | Avenger Precision Bass | 34.0″ | P: E/A | 28.0cm | 0.8235 | *28.0cm* | Tom6000 |  |
| 146 | Vantage | Avenger Precision Bass | 34.0″ | P: D/G | 31.0cm | 0.9118 | *31.0cm* | Tom6000 |  |
| 147 | Gibson | Les Paul Bass (1979) | 30.7″ | Neck | 22.2cm | 0.7231 | *24.6cm* | Tom6000 |  |
| 147 | Gibson | Les Paul Bass (1979) | 30.7″ | Bridge | 32.2cm | 1.0489 | *35.7cm* | Tom6000 |  |
| 148 | Klira | Kentucky | 30.0″ | Neck | 17.2cm | 0.5733 | *19.5cm* | walktheline |  |
| 148 | Klira | Kentucky | 30.0″ | Bridge | 28.7cm | 0.9567 | *32.5cm* | walktheline |  |
| 149 | Marleaux | JB-4 (active Jazz Bass) (1991) | 34.0″ | J: Neck | 27.7cm | 0.8147 | *27.7cm* | quarkfrosch |  |
| 149 | Marleaux | JB-4 (active Jazz Bass) (1991) | 34.0″ | J: Bridge | 37.0cm | 1.0882 | *37.0cm* | quarkfrosch |  |
| 150 | Ibanez | ATK405 (Made in Korea) (2001) | 34.0″ | J: Neck | 27.5cm | 0.8088 | *27.5cm* | quarkfrosch | Neck J + Bridge triple-coil humbucker, splitable, middle coil silent dummy |
| 150 | Ibanez | ATK405 (Made in Korea) (2001) | 34.0″ | Bridge, Coil 1 | 34.7cm | 1.0206 | *34.7cm* | quarkfrosch | Neck J + Bridge triple-coil humbucker, splitable, middle coil silent dummy |
| 150 | Ibanez | ATK405 (Made in Korea) (2001) | 34.0″ | Bridge, Coil 2 (dummy coil) | 36.2cm | 1.0647 | *36.2cm* | quarkfrosch | Neck J + Bridge triple-coil humbucker, splitable, middle coil silent dummy |
| 150 | Ibanez | ATK405 (Made in Korea) (2001) | 34.0″ | Bridge, Coil 3 | 37.7cm | 1.1088 | *37.7cm* | quarkfrosch | Neck J + Bridge triple-coil humbucker, splitable, middle coil silent dummy |
| 151 | Ibanez | ATK100 (Made in Japan) (1996) | 34.0″ | Bridge, Coil 1 | 33.2cm | 0.9765 | *33.2cm* | quarkfrosch |  |
| 151 | Ibanez | ATK100 (Made in Japan) (1996) | 34.0″ | Bridge, Coil 2 | 36.2cm | 1.0647 | *36.2cm* | quarkfrosch |  |
| 152 | Sire | P5R (Made in Indonesia) (2024) | 34.0″ | P: H/E/A | 28.1cm | 0.8265 | *28.1cm* | quarkfrosch |  |
| 152 | Sire | P5R (Made in Indonesia) (2024) | 34.0″ | P: D/G | 30.8cm | 0.9059 | *30.8cm* | quarkfrosch |  |
| 153 | Hagström | H II BN/F400N (1970s) | 30.7″ | Neck (only the visible polepieces were measured) | 19.6cm | 0.6384 | *21.7cm* | flamencito |  |
| 153 | Hagström | H II BN/F400N (1970s) | 30.7″ | Bridge (only the visible polepieces were measured) | 32.7cm | 1.0651 | *36.2cm* | flamencito |  |

{{< /table >}}
