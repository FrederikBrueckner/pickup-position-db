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
 * __Measurement__: the actual measurement. In a proper unit (cm; rounded to mm). Always measured from the 12th fret of the instrument. Usually measured to the pole piece. If the pole piece is not visible the edges of the pickup can be measured or an estimation can be made (add a __Comment__).
 * __Normalized__: the measurement scaled down to a virtual bass with a 1 inch scale (rounded to 4 decimal points). This makes measurements of basses with different scales comparable (and sortable). Multiply this value by the target scale (in inch) to get the position of the coil (in cm) measured from the 12th fret.
 * __Target 34/32/30__: The measurement scaled to a bass of scale 34, 32 or 30 respectively (in cm, rounded to mm)
 * __Reporter__: Name or handle of the person or user who provided the measurement
 * __Comment__: An additional comment if the measurement needs any

This project is published under the GPL v3 license. This explicitly includes the data of the following table.

{{< table sortable="true" searchable="true" >}}
|  #  | Brand | Make | Scale (inch) | Pickup/Coil | Measurement (cm) | Normalized | Target 34 (cm) | Target 32 (cm) | Target 30 (cm) | Reporter | Comment |
| --- | ----- | ---- | ------------ | ----------- | ---------------- | ---------- | -------------- | -------------- | -------------- | -------- | ------- |
| 1 | Fender | CIJ Jazzbass JB75-100NAT/R | 34.0 | J: Neck | 27.5 | 0.8088 | 27.5 | 25.9 | 24.3 | Doschd |  |
| 1 | Fender | CIJ Jazzbass JB75-100NAT/R | 34.0 | J: Bridge | 36.5 | 1.0735 | 36.5 | 34.4 | 32.2 | Doschd |  |
| 2 | Fender-like self-built | Jazz Bass | 34.0 | J: Neck | 28.0 | 0.8235 | 28.0 | 26.4 | 24.7 | BassManni |  |
| 2 | Fender-like self-built | Jazz Bass | 34.0 | J: Bridge | 36.6 | 1.0765 | 36.6 | 34.4 | 32.3 | BassManni |  |
| 3 | Fender | USA Jazz Standard S1 | 34.0 | J: Neck | 27.7 | 0.8147 | 27.7 | 26.1 | 24.4 | lenni |  |
| 3 | Fender | USA Jazz Standard S1 | 34.0 | J: Bridge | 36.8 | 1.0824 | 36.8 | 34.6 | 32.5 | lenni |  |
| 4 | Fender | CIJ Jazz Bass 62-83US | 34.0 | J: Neck | 27.7 | 0.8147 | 27.7 | 26.1 | 24.4 | griznak |  |
| 4 | Fender | CIJ Jazz Bass 62-83US | 34.0 | J: Bridge | 36.8 | 1.0824 | 36.8 | 34.6 | 32.5 | griznak |  |
| 5 | Fender | Jazzbass Fretless (1998) | 34.0 | J: Neck | 27.7 | 0.8147 | 27.7 | 26.1 | 24.4 | EMUBASS |  |
| 5 | Fender | Jazzbass Fretless (1998) | 34.0 | J: Bridge | 36.8 | 1.0824 | 36.8 | 34.6 | 32.5 | EMUBASS |  |
| 6 | Fender | Fender JB Special Edition 70s Spacing (1982?) | 34.0 | J: Neck | 27.7 | 0.8147 | 27.7 | 26.1 | 24.4 | BassManni |  |
| 6 | Fender | Fender JB Special Edition 70s Spacing (1982?) | 34.0 | J: Bridge | 37.7 | 1.1088 | 37.7 | 35.5 | 33.3 | BassManni |  |
| 7 | Johnson | Jazzbass | 34.0 | J: Neck | 27.8 | 0.8176 | 27.8 | 26.2 | 24.5 | BigB |  |
| 7 | Johnson | Jazzbass | 34.0 | J: Bridge | 38.0 | 1.1176 | 38.0 | 35.8 | 33.5 | BigB |  |
| 8 | Warwick | Streamer Stage 2 (2 J-Singlecoils) | 34.0 | J: Neck | 31.4 | 0.9235 | 31.4 | 29.6 | 27.7 | jam_bass |  |
| 8 | Warwick | Streamer Stage 2 (2 J-Singlecoils) | 34.0 | J: Bridge | 38.5 | 1.1324 | 38.5 | 36.2 | 34.0 | jam_bass |  |
| 9 | Fender | Jaguar Deluxe MIJ (2014) | 34.0 | J: Neck | 27.4 | 0.8059 | 27.4 | 25.8 | 24.2 | stoneface |  |
| 9 | Fender | Jaguar Deluxe MIJ (2014) | 34.0 | J: Bridge | 38.9 | 1.1441 | 38.9 | 36.6 | 34.3 | stoneface |  |
| 10 | Fenix | Jazzbass (1989) | 34.0 | J: Neck | 27.6 | 0.8118 | 27.6 | 26.0 | 24.4 | stoneface |  |
| 10 | Fenix | Jazzbass (1989) | 34.0 | J: Bridge | 36.7 | 1.0794 | 36.7 | 34.5 | 32.4 | stoneface |  |
| 11 | Guilber | ??? (1992) | 34.0 | J: Neck | 26.9 | 0.7912 | 26.9 | 25.3 | 23.7 | stoneface |  |
| 11 | Guilber | ??? (1992) | 34.0 | J: Bridge | 36.5 | 1.0735 | 36.5 | 34.4 | 32.2 | stoneface |  |
| 12 | Fender | Victor Bailey JB (2007) | 34.0 | J: Neck | 27.5 | 0.8088 | 27.5 | 25.9 | 24.3 | olebass |  |
| 12 | Fender | Victor Bailey JB (2007) | 34.0 | J: Bridge | 36.5 | 1.0735 | 36.5 | 34.4 | 32.2 | olebass |  |
| 13 | Lodestone | Primal Artist V JB | 34.0 | J: Neck | 28.0 | 0.8235 | 28.0 | 26.4 | 24.7 | olebass |  |
| 13 | Lodestone | Primal Artist V JB | 34.0 | J: Bridge | 37.0 | 1.0882 | 37.0 | 34.8 | 32.6 | olebass |  |
| 14 | Sandberg | California JM V (2005) | 34.0 | J: Neck | 28.0 | 0.8235 | 28.0 | 26.4 | 24.7 | olebass |  |
| 14 | Sandberg | California JM V (2005) | 34.0 | J: Bridge | 36.9 | 1.0853 | 36.9 | 34.7 | 32.6 | olebass |  |
| 15 | Vester | Clipper IV (1990) | 34.0 | J: Neck | 30.0 | 0.8824 | 30.0 | 28.2 | 26.5 | olebass |  |
| 15 | Vester | Clipper IV (1990) | 34.0 | J: Bridge | 37.5 | 1.1029 | 37.5 | 35.3 | 33.1 | olebass |  |
| 16 | Yamaha | BB Standard (1997) | 34.0 | J: Neck | 27.2 | 0.8000 | 27.2 | 25.6 | 24.0 | Youth |  |
| 16 | Yamaha | BB Standard (1997) | 34.0 | J: Bridge | 36.4 | 1.0706 | 36.4 | 34.3 | 32.1 | Youth |  |
| 17 | Yamaha | BB604 | 34.0 | J: Neck | 30.5 | 0.8971 | 30.5 | 28.7 | 26.9 | Youth |  |
| 17 | Yamaha | BB604 | 34.0 | J: Bridge | 38.2 | 1.1235 | 38.2 | 36.0 | 33.7 | Youth |  |
| 18 | Fender | Jaguar MIJ | 34.0 | J: Neck | 27.3 | 0.8029 | 27.3 | 25.7 | 24.1 | Youth |  |
| 18 | Fender | Jaguar MIJ | 34.0 | J: Bridge | 37.7 | 1.1088 | 37.7 | 35.5 | 33.3 | Youth |  |
| 19 | BolleBass | Corinna (2020) | 32.67 | J: Neck | 27.9 | 0.8540 | 29.0 | 27.3 | 25.6 | bassilisk |  |
| 19 | BolleBass | Corinna (2020) | 32.67 | J: Bridge | 36.9 | 1.1295 | 38.4 | 36.1 | 33.9 | bassilisk |  |
| 20 | Sandberg | TT (2013) | 34.0 | J: Neck | 27.7 | 0.8147 | 27.7 | 26.1 | 24.4 | Seven Basses |  |
| 20 | Sandberg | TT (2013) | 34.0 | J: Bridge | 36.7 | 1.0794 | 36.7 | 34.5 | 32.4 | Seven Basses |  |
| 21 | Marleaux | Votan X-Tra (2017) | 34.0 | J: Neck | 27.5 | 0.8088 | 27.5 | 25.9 | 24.3 | Seven Basses |  |
| 21 | Marleaux | Votan X-Tra (2017) | 34.0 | J: Bridge | 36.5 | 1.0735 | 36.5 | 34.4 | 32.2 | Seven Basses |  |
| 22 | Squier | Affinity Precision - 20th Anniversary | 34.0 | P: E/A | 28.1 | 0.8265 | 28.1 | 26.4 | 24.8 | Axel |  |
| 22 | Squier | Affinity Precision - 20th Anniversary | 34.0 | P: D/G | 30.9 | 0.9088 | 30.9 | 29.1 | 27.3 | Axel |  |
| 23 | Self-Built | Precision | 34.0 | P: E/A | 28.1 | 0.8265 | 28.1 | 26.4 | 24.8 | stoneface |  |
| 23 | Self-Built | Precision | 34.0 | P: D/G | 30.9 | 0.9088 | 30.9 | 29.1 | 27.3 | stoneface |  |
| 24 | Fender | Precision Road Worn (2011) | 34.0 | P: E/A | 28.4 | 0.8353 | 28.4 | 26.7 | 25.1 | indian66 |  |
| 24 | Fender | Precision Road Worn (2011) | 34.0 | P: D/G | 31.1 | 0.9147 | 31.1 | 29.3 | 27.4 | indian66 |  |
| 25 | Fender | Precision (1972) | 34.0 | P: E/A | 28.3 | 0.8324 | 28.3 | 26.6 | 25.0 | Nick McNoise |  |
| 25 | Fender | Precision (1972) | 34.0 | P: D/G | 30.8 | 0.9059 | 30.8 | 29.0 | 27.2 | Nick McNoise |  |
| 26 | Fender | 51 Model CIJ (Singlecoil) (2006) | 34.0 | P Singlecoil | 26.2 | 0.7706 | 26.2 | 24.7 | 23.1 |  |  |
| 27 | Aria | Pro II RSB-Deluxe (Revers P) (1983) | 34.0 | P: E/A | 33.8 | 0.9941 | 33.8 | 31.8 | 29.8 | Youth |  |
| 27 | Aria | Pro II RSB-Deluxe (Revers P) (1983) | 34.0 | P: D/G | 32.1 | 0.9441 | 32.1 | 30.2 | 28.3 | Youth |  |
| 28 | Fenix | Precision (1991) | 34.0 | P: E/A | 28.3 | 0.8324 | 28.3 | 26.6 | 25.0 | mosys |  |
| 28 | Fenix | Precision (1991) | 34.0 | P: D/G | 31.0 | 0.9118 | 31.0 | 29.2 | 27.4 | mosys |  |
| 29 | Travis Bean | TB 2000 (1977) | 33.25 | SC: Neck | 18.9 | 0.5684 | 19.3 | 18.2 | 17.1 | quarkfrosch |  |
| 29 | Travis Bean | TB 2000 (1977) | 33.25 | SC: Bridge | 32.0 | 0.9624 | 32.7 | 30.8 | 28.9 | quarkfrosch |  |
| 30 | Kramer | XKB-20 (1980) | 33.75 | P: E/A | 27.4 | 0.8119 | 27.6 | 26.0 | 24.4 | quarkfrosch |  |
| 30 | Kramer | XKB-20 (1980) | 33.75 | P: D/G | 30.4 | 0.9007 | 30.6 | 28.8 | 27.0 | quarkfrosch |  |
| 31 | Vantage | VP-795B (1979) | 33.25 | P: Neck E/A | 22.3 | 0.6707 | 22.8 | 21.5 | 20.1 | quarkfrosch |  |
| 31 | Vantage | VP-795B (1979) | 33.25 | P: Neck D/G | 25.7 | 0.7729 | 26.3 | 24.7 | 23.2 | quarkfrosch |  |
| 31 | Vantage | VP-795B (1979) | 33.25 | P: Bridge E/A | 31.7 | 0.9534 | 32.4 | 30.5 | 28.6 | quarkfrosch |  |
| 31 | Vantage | VP-795B (1979) | 33.25 | P: Bridge D/G | 34.6 | 1.0406 | 35.4 | 33.3 | 31.2 | quarkfrosch |  |
| 32 | Warwick | Corvette Std (2003) | 34.0 | J: Neck | 29.8 | 0.8765 | 29.8 | 28.0 | 26.3 | quarkfrosch |  |
| 32 | Warwick | Corvette Std (2003) | 34.0 | 2J: Bridge Coil 1 | 38.0 | 1.1176 | 38.0 | 35.8 | 33.5 | quarkfrosch |  |
| 32 | Warwick | Corvette Std (2003) | 34.0 | 2J: Bridge Coil 2 | 40.4 | 1.1882 | 40.4 | 38.0 | 35.6 | quarkfrosch |  |
| 33 | Warwick | Fortress One (1994) | 34.0 | P: E/A | 30.2 | 0.8882 | 30.2 | 28.4 | 26.6 | quarkfrosch |  |
| 33 | Warwick | Fortress One (1994) | 34.0 | P: D/G | 33.0 | 0.9706 | 33.0 | 31.1 | 29.1 | quarkfrosch |  |
| 33 | Warwick | Fortress One (1994) | 34.0 | J: Bridge | 38.6 | 1.1353 | 38.6 | 36.3 | 34.1 | quarkfrosch |  |
| 34 | Charvel | 2B (1988) | 34.0 | P: E/A | 28.3 | 0.8324 | 28.3 | 26.6 | 25.0 | quarkfrosch |  |
| 34 | Charvel | 2B (1988) | 34.0 | P: D/G | 31.2 | 0.9176 | 31.2 | 29.4 | 27.5 | quarkfrosch |  |
| 34 | Charvel | 2B (1988) | 34.0 | J: Bridge | 37.7 | 1.1088 | 37.7 | 35.5 | 33.3 | quarkfrosch |  |
| 35 | Fenix | P-Bass (1991) | 34.0 | P: E/A | 28.2 | 0.8294 | 28.2 | 26.5 | 24.9 | quarkfrosch |  |
| 35 | Fenix | P-Bass (1991) | 34.0 | P: D/G | 31.1 | 0.9147 | 31.1 | 29.3 | 27.4 | quarkfrosch |  |
| 35 | Fenix | P-Bass (1991) | 34.0 | J: Bridge | 38.4 | 1.1294 | 38.4 | 36.1 | 33.9 | quarkfrosch |  |
| 36 | Fenix | Jazz-Bass (1994) | 34.0 | J: Neck | 27.5 | 0.8088 | 27.5 | 25.9 | 24.3 | quarkfrosch |  |
| 36 | Fenix | Jazz-Bass (1994) | 34.0 | J: Bridge | 36.7 | 1.0794 | 36.7 | 34.5 | 32.4 | quarkfrosch |  |
| 37 | Fenix | Jazz-Bass V (1992) | 34.0 | J: Neck | 27.5 | 0.8088 | 27.5 | 25.9 | 24.3 | quarkfrosch |  |
| 37 | Fenix | Jazz-Bass V (1992) | 34.0 | J: Bridge | 36.7 | 1.0794 | 36.7 | 34.5 | 32.4 | quarkfrosch |  |
| 38 | Squier | Jaguar VM (2014) | 30.0 | P: E/A | 22.8 | 0.7600 | 25.8 | 24.3 | 22.8 | quarkfrosch |  |
| 38 | Squier | Jaguar VM (2014) | 30.0 | P: D/G | 25.8 | 0.8600 | 29.2 | 27.5 | 25.8 | quarkfrosch |  |
| 38 | Squier | Jaguar VM (2014) | 30.0 | J: Bridge | 33.7 | 1.1233 | 38.2 | 35.9 | 33.7 | quarkfrosch |  |
| 39 | Mayones | Slogan Classic 4 (2009) | 34.25 | HB: Neck Coil 1 | 27.9 | 0.8146 | 27.7 | 26.1 | 24.4 | quarkfrosch |  |
| 39 | Mayones | Slogan Classic 4 (2009) | 34.25 | HB: Neck Coil 2 | 20.5 | 0.5985 | 20.3 | 19.2 | 18.0 | quarkfrosch |  |
| 39 | Mayones | Slogan Classic 4 (2009) | 34.25 | HB: Bridge Coil 1 | 36.7 | 1.0715 | 36.4 | 34.3 | 32.1 | quarkfrosch |  |
| 39 | Mayones | Slogan Classic 4 (2009) | 34.25 | HB: Bridge Coil 2 | 39.3 | 1.1474 | 39.0 | 36.7 | 34.4 | quarkfrosch |  |
| 40 | OLP | Tony Levin 5 (2005) | 34.0 | MM: Bridge Coil 1 | 33.9 | 0.9971 | 33.9 | 31.9 | 29.9 | quarkfrosch |  |
| 40 | OLP | Tony Levin 5 (2005) | 34.0 | MM: Bridge Coil 2 | 39.2 | 1.1529 | 39.2 | 36.9 | 34.6 | quarkfrosch |  |
| 41 | Bass Collection | SB302 PWH (1987) | 34.25 | P: E/A | 30.1 | 0.8788 | 29.9 | 28.1 | 26.4 | quarkfrosch |  |
| 41 | Bass Collection | SB302 PWH (1987) | 34.25 | P: D/G | 33.2 | 0.9693 | 33.0 | 31.0 | 29.1 | quarkfrosch |  |
| 41 | Bass Collection | SB302 PWH (1987) | 34.25 | J: Bridge | 38.1 | 1.1124 | 37.8 | 35.6 | 33.4 | quarkfrosch |  |
| 42 | Wotan | Shark (1978) | 34.5 | P: Neck E/A | 21.0 | 0.6087 | 20.7 | 19.5 | 18.3 | quarkfrosch |  |
| 42 | Wotan | Shark (1978) | 34.5 | P: Neck D/G | 23.7 | 0.6870 | 23.4 | 22.0 | 20.6 | quarkfrosch |  |
| 42 | Wotan | Shark (1978) | 34.5 | P: Bridge E/A | 34.2 | 0.9913 | 33.7 | 31.7 | 29.7 | quarkfrosch |  |
| 42 | Wotan | Shark (1978) | 34.5 | P: Bridge D/G | 37.7 | 1.0928 | 37.2 | 35.0 | 32.8 | quarkfrosch |  |
| 43 | Kramer | DMZ 5000 (1980) | 33.75 | P: Neck E/A | 27.5 | 0.8148 | 27.7 | 26.1 | 24.4 | quarkfrosch |  |
| 43 | Kramer | DMZ 5000 (1980) | 33.75 | P: Neck D/G | 30.3 | 0.8978 | 30.5 | 28.7 | 26.9 | quarkfrosch |  |
| 43 | Kramer | DMZ 5000 (1980) | 33.75 | J: Bridge | 39.0 | 1.1556 | 39.3 | 37.0 | 34.7 | quarkfrosch |  |
| 44 | Esh | Stinger I | 34.25 | HB: Neck Coil 1 | 31.1 | 0.9080 | 30.9 | 29.1 | 27.2 | quarkfrosch |  |
| 44 | Esh | Stinger I | 34.25 | HB: Neck Coil 1 | 33.1 | 0.9664 | 32.9 | 30.9 | 29.0 | quarkfrosch |  |
| 44 | Esh | Stinger I | 34.25 | HB: Bridge Coil 1 | 37.7 | 1.1007 | 37.4 | 35.2 | 33.0 | quarkfrosch |  |
| 44 | Esh | Stinger I | 34.25 | HB: Bridge Coil 2 | 39.7 | 1.1591 | 39.4 | 37.1 | 34.8 | quarkfrosch |  |
| 45 | Esh | Stinger II - V | 34.25 | HB: Neck Coil 1 | 29.0 | 0.8467 | 28.8 | 27.1 | 25.4 | quarkfrosch |  |
| 45 | Esh | Stinger II - V | 34.25 | HB: Neck Coil 2 | 31.0 | 0.9051 | 30.8 | 29.0 | 27.2 | quarkfrosch |  |
| 45 | Esh | Stinger II - V | 34.25 | HB: Bridge Coil 1 | 36.8 | 1.0745 | 36.5 | 34.4 | 32.2 | quarkfrosch |  |
| 45 | Esh | Stinger II - V | 34.25 | HB: Bridge Coil 2 | 38.8 | 1.1328 | 38.5 | 36.2 | 34.0 | quarkfrosch |  |
| 46 | Daion | Power Mark X-B (1982) | 34.0 | P: E/A | 30.2 | 0.8882 | 30.2 | 28.4 | 26.6 | quarkfrosch |  |
| 46 | Daion | Power Mark X-B (1982) | 34.0 | P: D/G | 33.0 | 0.9706 | 33.0 | 31.1 | 29.1 | quarkfrosch |  |
| 47 | Gibson | Thunderbird (2005) | 34.0 | Neck | 26.0 | 0.7647 | 26.0 | 24.5 | 22.9 | Oli Wan | Only the middle of the humbucker was measured |
| 47 | Gibson | Thunderbird (2005) | 34.0 | Bridge | 37.0 | 1.0882 | 37.0 | 34.8 | 32.6 | Oli Wan | Only the middle of the humbucker was measured |
| 48 | Ibanez | TR Expressionist 500 | 34.0 | Neck | 26.9 | 0.7912 | 26.9 | 25.3 | 23.7 | lenni | Only the middle of the humbucker was measured |
| 48 | Ibanez | TR Expressionist 500 | 34.0 | Bridge | 37.0 | 1.0882 | 37.0 | 34.8 | 32.6 | lenni | Only the middle of the humbucker was measured |
| 49 | Ibanez | SR-500 | 34.0 | Neck | 31.0 | 0.9118 | 31.0 | 29.2 | 27.4 | Doschd | Only the middle of the humbucker was measured |
| 49 | Ibanez | SR-500 | 34.0 | Bridge | 38.7 | 1.1382 | 38.7 | 36.4 | 34.1 | Doschd | Only the middle of the humbucker was measured |
| 50 | Ibanez | BTB675 | 35.0 | Neck | 29.1 | 0.8314 | 28.3 | 26.6 | 24.9 | TheBrian | Only the middle of the humbucker was measured |
| 50 | Ibanez | BTB675 | 35.0 | Bridge | 38.4 | 1.0971 | 37.3 | 35.1 | 32.9 | TheBrian | Only the middle of the humbucker was measured |
| 51 | Lakland | Skyline 55-01 | 35.0 | Neck | 27.6 | 0.7886 | 26.8 | 25.2 | 23.7 | EMUBASS | Only the middle of the humbucker was measured. Bartolini Humbucker |
| 51 | Lakland | Skyline 55-01 | 35.0 | Bridge | 35.9 | 1.0257 | 34.9 | 32.8 | 30.8 | EMUBASS | Only the middle of the humbucker was measured. Bartolini Humbucker |
| 52 | Epiphone | Thunderbird Classic IV Pro (2015) | 34.0 | Neck | 25.7 | 0.7559 | 25.7 | 24.2 | 22.7 | stoneface | Only the middle of the humbucker was measured |
| 52 | Epiphone | Thunderbird Classic IV Pro (2015) | 34.0 | Bridge | 36.7 | 1.0794 | 36.7 | 34.5 | 32.4 | stoneface | Only the middle of the humbucker was measured |
| 53 | Esh | Sovereign | 34.0 | Neck | 30.0 | 0.8824 | 30.0 | 28.2 | 26.5 | Stratitis | Only the middle of the humbucker was measured. Soapbar, EMG35DC |
| 53 | Esh | Sovereign | 34.0 | Bridge | 36.4 | 1.0706 | 36.4 | 34.3 | 32.1 | Stratitis | Only the middle of the humbucker was measured. Soapbar, EMG35DC |
| 54 | Epiphone | Thunderbird Classic IV Pro (2018) | 34.0 | Neck | 25.7 | 0.7559 | 25.7 | 24.2 | 22.7 | indian66 | Only the middle of the humbucker was measured |
| 54 | Epiphone | Thunderbird Classic IV Pro (2018) | 34.0 | Bridge | 36.7 | 1.0794 | 36.7 | 34.5 | 32.4 | indian66 | Only the middle of the humbucker was measured |
| 55 | Esh | Sovereign V | 34.0 | Neck | 31.8 | 0.9353 | 31.8 | 29.9 | 28.1 | aBaxxi | Only the middle of the humbucker was measured. Soapbar. Probably BD5S |
| 55 | Esh | Sovereign V | 34.0 | Bridge | 38.3 | 1.1265 | 38.3 | 36.0 | 33.8 | aBaxxi | Only the middle of the humbucker was measured. Soapbar. Probably BD5S |
| 56 | Ibanez | RB850 (1986) | 34.0 | Neck | 29.5 | 0.8676 | 29.5 | 27.8 | 26.0 | boeb | Only the middle of the humbucker was measured |
| 56 | Ibanez | RB850 (1986) | 34.0 | Bridge | 38.4 | 1.1294 | 38.4 | 36.1 | 33.9 | boeb | Only the middle of the humbucker was measured |
| 57 | Washburn | Bantham XB400 | 34.0 | Neck | 30.5 | 0.8971 | 30.5 | 28.7 | 26.9 | Youth | Only the middle of the humbucker was measured |
| 57 | Washburn | Bantham XB400 | 34.0 | Bridge | 37.5 | 1.1029 | 37.5 | 35.3 | 33.1 | Youth | Only the middle of the humbucker was measured |
| 58 | von Paris | Piccolo (1999) | 30.0 | Neck | 29.0 | 0.9667 | 32.9 | 30.9 | 29.0 | Papa | Only the middle of the humbucker was measured |
| 58 | von Paris | Piccolo (1999) | 30.0 | Bridge | 38.4 | 1.2800 | 43.5 | 41.0 | 38.4 | Papa | Only the middle of the humbucker was measured |
| 59 | Yamaha | BBG4S II (2001) | 34.0 | Neck | 29.7 | 0.8735 | 29.7 | 28.0 | 26.2 | Seven Basses | Only the middle of the humbucker was measured |
| 59 | Yamaha | BBG4S II (2001) | 34.0 | Bridge | 37.7 | 1.1088 | 37.7 | 35.5 | 33.3 | Seven Basses | Only the middle of the humbucker was measured |
| 60 | Basscollection | SB301 | 34.0 | P: Neck E/A | 29.5 | 0.8676 | 29.5 | 27.8 | 26.0 | mulhofa |  |
| 60 | Basscollection | SB301 | 34.0 | P: Neck D/G | 32.0 | 0.9412 | 32.0 | 30.1 | 28.2 | mulhofa |  |
| 60 | Basscollection | SB301 | 34.0 | J: Bridge | 37.5 | 1.1029 | 37.5 | 35.3 | 33.1 | mulhofa |  |
| 61 | Squier Hohner | Frankenstein | 34.0 | P: Neck E/A | 26.9 | 0.7912 | 26.9 | 25.3 | 23.7 | EMUBASS |  |
| 61 | Squier Hohner | Frankenstein | 34.0 | P: Neck D/G | 30.0 | 0.8824 | 30.0 | 28.2 | 26.5 | EMUBASS |  |
| 61 | Squier Hohner | Frankenstein | 34.0 | J: Bridge | 38.0 | 1.1176 | 38.0 | 35.8 | 33.5 | EMUBASS |  |
| 62 | Warwick | Streamer LX-4 | 34.0 | P: Neck E/A | 33.0 | 0.9706 | 33.0 | 31.1 | 29.1 | Doschd |  |
| 62 | Warwick | Streamer LX-4 | 34.0 | P: Neck D/G | 30.0 | 0.8824 | 30.0 | 28.2 | 26.5 | Doschd |  |
| 62 | Warwick | Streamer LX-4 | 34.0 | J: Bridge | 38.5 | 1.1324 | 38.5 | 36.2 | 34.0 | Doschd |  |
| 63 | Warwick | Streamer One MEC (1990) | 34.0 | P: Neck E/A | 33.2 | 0.9765 | 33.2 | 31.2 | 29.3 | EMUBASS |  |
| 63 | Warwick | Streamer One MEC (1990) | 34.0 | P: Neck D/G | 30.3 | 0.8912 | 30.3 | 28.5 | 26.7 | EMUBASS |  |
| 63 | Warwick | Streamer One MEC (1990) | 34.0 | J: Bridge | 38.7 | 1.1382 | 38.7 | 36.4 | 34.1 | EMUBASS |  |
| 64 | Warwick | Fortress One MEC (1993) | 34.0 | P: Neck E/A | 30.4 | 0.8941 | 30.4 | 28.6 | 26.8 | EMUBASS |  |
| 64 | Warwick | Fortress One MEC (1993) | 34.0 | P: Neck D/G | 33.3 | 0.9794 | 33.3 | 31.3 | 29.4 | EMUBASS |  |
| 64 | Warwick | Fortress One MEC (1993) | 34.0 | J: Bridge | 38.9 | 1.1441 | 38.9 | 36.6 | 34.3 | EMUBASS |  |
| 65 | Warwick | Fortress One | 34.0 | P: Neck E/A | 30.0 | 0.8824 | 30.0 | 28.2 | 26.5 | Doschd |  |
| 65 | Warwick | Fortress One | 34.0 | P: Neck D/G | 33.0 | 0.9706 | 33.0 | 31.1 | 29.1 | Doschd |  |
| 65 | Warwick | Fortress One | 34.0 | J: Bridge | 41.0 | 1.2059 | 41.0 | 38.6 | 36.2 | Doschd | Measurment was marked as questionable in the old list |
| 66 | HotWire | Prezzman (2005) | 34.0 | P: Neck E/A | 28.5 | 0.8382 | 28.5 | 26.8 | 25.1 | olebass |  |
| 66 | HotWire | Prezzman (2005) | 34.0 | P: Neck D/G | 31.5 | 0.9265 | 31.5 | 29.6 | 27.8 | olebass |  |
| 66 | HotWire | Prezzman (2005) | 34.0 | J: Bridge | 37.2 | 1.0941 | 37.2 | 35.0 | 32.8 | olebass |  |
| 67 | Ibanez | Destroyer DT650 (1982) | 34.0 | P: Neck E/A | 25.0 | 0.7353 | 25.0 | 23.5 | 22.1 | boeb |  |
| 67 | Ibanez | Destroyer DT650 (1982) | 34.0 | P: Neck D/G | 28.2 | 0.8294 | 28.2 | 26.5 | 24.9 | boeb |  |
| 67 | Ibanez | Destroyer DT650 (1982) | 34.0 | J: Bridge | 35.5 | 1.0441 | 35.5 | 33.4 | 31.3 | boeb |  |
| 68 | Fender | Vintage Hot Rod 70s Jazz Bass | 34.0 | P: Neck E/A | 25.8 | 0.7588 | 25.8 | 24.3 | 22.8 | Xaver |  |
| 68 | Fender | Vintage Hot Rod 70s Jazz Bass | 34.0 | P: Neck D/G | 28.5 | 0.8382 | 28.5 | 26.8 | 25.1 | Xaver |  |
| 68 | Fender | Vintage Hot Rod 70s Jazz Bass | 34.0 | J: Bridge | 38.0 | 1.1176 | 38.0 | 35.8 | 33.5 | Xaver |  |
| 69 | Yamaha | BB414 | 34.0 | P: Neck E/A | 27.8 | 0.8176 | 27.8 | 26.2 | 24.5 | stoneface |  |
| 69 | Yamaha | BB414 | 34.0 | P: Neck D/G | 30.9 | 0.9088 | 30.9 | 29.1 | 27.3 | stoneface |  |
| 69 | Yamaha | BB414 | 34.0 | J: Bridge | 37.5 | 1.1029 | 37.5 | 35.3 | 33.1 | stoneface |  |
| 70 | Ibanez | Blazer | 34.0 | P: Neck E/A | 25.7 | 0.7559 | 25.7 | 24.2 | 22.7 | mosys |  |
| 70 | Ibanez | Blazer | 34.0 | P: Neck D/G | 28.5 | 0.8382 | 28.5 | 26.8 | 25.1 | mosys |  |
| 70 | Ibanez | Blazer | 34.0 | J: Bridge | 35.9 | 1.0559 | 35.9 | 33.8 | 31.7 | mosys |  |
| 71 | Yamaha | BB1024X (2012) | 34.0 | P: Neck E/A | 28.0 | 0.8235 | 28.0 | 26.4 | 24.7 | Seven Basses |  |
| 71 | Yamaha | BB1024X (2012) | 34.0 | P: Neck D/G | 31.0 | 0.9118 | 31.0 | 29.2 | 27.4 | Seven Basses |  |
| 71 | Yamaha | BB1024X (2012) | 34.0 | J: Bridge | 37.5 | 1.1029 | 37.5 | 35.3 | 33.1 | Seven Basses |  |
| 72 | Squier | Affinity P (2014) | 34.0 | P: Neck E/A | 28.0 | 0.8235 | 28.0 | 26.4 | 24.7 | Seven Basses | with EMG GZR |
| 72 | Squier | Affinity P (2014) | 34.0 | P: Neck D/G | 31.0 | 0.9118 | 31.0 | 29.2 | 27.4 | Seven Basses | with EMG GZR |
| 72 | Squier | Affinity P (2014) | 34.0 | J: Bridge | 38.0 | 1.1176 | 38.0 | 35.8 | 33.5 | Seven Basses | with EMG GZR |
| 73 | Dingwall | SP-1 5-string | 34.8 | P: Neck-Coil. B-String  | 29.0 | 0.8333 | 28.3 | 26.7 | 25.0 | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 34.8 | J: Bridge. B-String | 36.0 | 1.0345 | 35.2 | 33.1 | 31.0 | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 34.02 | P: Neck-Coil. E-String | 30.5 | 0.8965 | 30.5 | 28.7 | 26.9 | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 34.02 | J: Bridge. E-String | 35.5 | 1.0435 | 35.5 | 33.4 | 31.3 | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 33.39 | P: Bridge-Coil. A-String | 30.0 | 0.8985 | 30.5 | 28.8 | 27.0 | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 33.39 | J: Bridge. A-String | 35.0 | 1.0482 | 35.6 | 33.5 | 31.4 | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 32.68 | P: Bridge-Coil. D-String | 30.6 | 0.9364 | 31.8 | 30.0 | 28.1 | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 32.68 | J: Bridge. D-String | 35.4 | 1.0832 | 36.8 | 34.7 | 32.5 | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 31.89 | P: Bridge-Coil. G-String | 29.0 | 0.9094 | 30.9 | 29.1 | 27.3 | foxtrott |  |
| 73 | Dingwall | SP-1 5-string | 31.89 | J: Bridge. G-String | 34.0 | 1.0662 | 36.3 | 34.1 | 32.0 | foxtrott |  |
| 74 | Höfner | 500/1 | 30.0 | Neck | 34.5 | 1.1500 | 39.1 | 36.8 | 34.5 | Oli Wan |  |
| 74 | Höfner | 500/1 | 30.0 | Bridge | 39.4 | 1.3133 | 44.7 | 42.0 | 39.4 | Oli Wan |  |
| 75 | Rickenbacker | 4003/V63 (1991) | 33.25 | Neck | 21.7 | 0.6526 | 22.2 | 20.9 | 19.6 | Oli Wan |  |
| 75 | Rickenbacker | 4003/V63 (1991) | 33.25 | Bridge | 32.7 | 0.9835 | 33.4 | 31.5 | 29.5 | Oli Wan |  |
| 76 | Sandberg | California MM-5 | 35.0 | Neck | 21.7 | 0.6200 | 21.1 | 19.8 | 18.6 | Doschd | Only the middle of the humbucker was measured |
| 76 | Sandberg | California MM-5 | 35.0 | Bridge | 32.7 | 0.9343 | 31.8 | 29.9 | 28.0 | Doschd | Only the middle of the humbucker was measured |
| 77 | MusicMan | Sterling 4-String | 34.0 | MM | 34.3 | 1.0088 | 34.3 | 32.3 | 30.3 | Bassonovo | Only the middle of the humbucker was measured |
| 78 | Ibanez | ATK 300 | 34.0 | Humbucker | 34.7 | 1.0206 | 34.7 | 32.7 | 30.6 | griznak | Triplecoil with dummy-coil. Only the middle of the humbucker was measured |
| 79 | Warwick | Rockbass Corvette $$ | 34.0 | Neck | 29.7 | 0.8735 | 29.7 | 28.0 | 26.2 | EMUBASS | Not clear whether the model had soapbars or singlecoils. |
| 79 | Warwick | Rockbass Corvette $$ | 34.0 | Bridge | 35.7 | 1.0500 | 35.7 | 33.6 | 31.5 | EMUBASS | Not clear whether the model had soapbars or singlecoils. |
| 80 | Rickenbacker | 4003 (2010) | 33.25 | Neck | 21.6 | 0.6496 | 22.1 | 20.8 | 19.5 | stoneface |  |
| 80 | Rickenbacker | 4003 (2010) | 33.25 | Bridge | 32.9 | 0.9895 | 33.6 | 31.7 | 29.7 | stoneface |  |
| 81 | Danelectro | 63 Longscale | 34.0 | Neck | 28.2 | 0.8294 | 28.2 | 26.5 | 24.9 | Axel |  |
| 81 | Danelectro | 63 Longscale | 34.0 | Bridge | 38.5 | 1.1324 | 38.5 | 36.2 | 34.0 | Axel |  |
| 82 | Career | Stingray Copy | 34.0 | MM | 34.8 | 1.0235 | 34.8 | 32.8 | 30.7 | triple-U | Only the middle of the humbucker was measured |
| 83 | Esh | Notorious 2 V (2005) | 34.0 | MM | 35.0 | 1.0294 | 35.0 | 32.9 | 30.9 | olebass | Only the middle of the humbucker was measured |
| 84 | Rickenbacker | 4003S5 (2019) | 33.25 | Neck | 21.7 | 0.6526 | 22.2 | 20.9 | 19.6 | Mc Valve |  |
| 84 | Rickenbacker | 4003S5 (2019) | 33.25 | Bridge | 35.3 | 1.0617 | 36.1 | 34.0 | 31.9 | Mc Valve |  |
| 85 | Fender | Jazz Bass V - Roscoe Beck (1996) | 34.0 | Neck PU; Neck Coil | 27.8 | 0.8176 | 27.8 | 26.2 | 24.5 | Lowender | Double J Humbucker. Measurments were taken from a photo and might be off. |
| 85 | Fender | Jazz Bass V - Roscoe Beck (1996) | 34.0 | Neck PU; Bridge Coil | 29.4 | 0.8647 | 29.4 | 27.7 | 25.9 | Lowender | Double J Humbucker. Measurments were taken from a photo and might be off. |
| 85 | Fender | Jazz Bass V - Roscoe Beck (1996) | 34.0 | Bridge PU; Neck Coil | 36.9 | 1.0853 | 36.9 | 34.7 | 32.6 | Lowender | Double J Humbucker. Measurments were taken from a photo and might be off. |
| 85 | Fender | Jazz Bass V - Roscoe Beck (1996) | 34.0 | Bridge PU; Bridge Coil | 38.5 | 1.1324 | 38.5 | 36.2 | 34.0 | Lowender | Double J Humbucker. Measurments were taken from a photo and might be off. |
| 86 | MusicMan | Stingray 4 HH | 34.0 | Neck PU; Neck Coil | 25.3 | 0.7441 | 25.3 | 23.8 | 22.3 | Marten |  |
| 86 | MusicMan | Stingray 4 HH | 34.0 | Neck PU; Bridge Coil | 27.7 | 0.8147 | 27.7 | 26.1 | 24.4 | Marten |  |
| 86 | MusicMan | Stingray 4 HH | 34.0 | Bridge PU; Neck Coil | 34.4 | 1.0118 | 34.4 | 32.4 | 30.4 | Marten |  |
| 86 | MusicMan | Stingray 4 HH | 34.0 | Bridge PU; Bridge Coil | 36.8 | 1.0824 | 36.8 | 34.6 | 32.5 | Marten |  |
| 87 | MusicMan | Stingray 5 H (2009) | 34.0 | MM Neck Coil | 33.1 | 0.9735 | 33.1 | 31.2 | 29.2 | 4enima |  |
| 87 | MusicMan | Stingray 5 H (2009) | 34.0 | MM Bridge Coil | 35.4 | 1.0412 | 35.4 | 33.3 | 31.2 | 4enima |  |
| 88 | Harley Benton | Marquess-4 | 34.0 | P: E/A | 29.5 | 0.8676 | 29.5 | 27.8 | 26.0 | Dommbasstisch |  |
| 88 | Harley Benton | Marquess-4 | 34.0 | P: D/G | 32.3 | 0.9500 | 32.3 | 30.4 | 28.5 | Dommbasstisch |  |
| 88 | Harley Benton | Marquess-4 | 34.0 | J: Bridge | 38.6 | 1.1353 | 38.6 | 36.3 | 34.1 | Dommbasstisch |  |
| 89 | Harley Benton | PJ-4 SBK | 34.0 | P: E/A | 28.5 | 0.8382 | 28.5 | 26.8 | 25.1 | Dommbasstisch |  |
| 89 | Harley Benton | PJ-4 SBK | 34.0 | P: D/G | 31.3 | 0.9206 | 31.3 | 29.5 | 27.6 | Dommbasstisch |  |
| 89 | Harley Benton | PJ-4 SBK | 34.0 | J: Bridge | 38.5 | 1.1324 | 38.5 | 36.2 | 34.0 | Dommbasstisch |  |
| 90 | Yamaha | RBX374 | 34.0 | HB: Neck Coil 1 | 26.9 | 0.7912 | 26.9 | 25.3 | 23.7 | Dommbasstisch |  |
| 90 | Yamaha | RBX374 | 34.0 | HB: Neck Coil 2 | 28.7 | 0.8441 | 28.7 | 27.0 | 25.3 | Dommbasstisch |  |
| 90 | Yamaha | RBX374 | 34.0 | HB: Bridge Coil 1 | 34.9 | 1.0265 | 34.9 | 32.8 | 30.8 | Dommbasstisch |  |
| 90 | Yamaha | RBX374 | 34.0 | HB: Bridge Coil 2 | 36.5 | 1.0735 | 36.5 | 34.4 | 32.2 | Dommbasstisch |  |
| 91 | Ibanez | BTB200 | 35.0 | HB: Neck Coil 1 | 26.7 | 0.7629 | 25.9 | 24.4 | 22.9 | Dommbasstisch |  |
| 91 | Ibanez | BTB200 | 35.0 | HB: Neck Coil 2 | 29.5 | 0.8429 | 28.7 | 27.0 | 25.3 | Dommbasstisch |  |
| 91 | Ibanez | BTB200 | 35.0 | HB: Bridge Coil 1 | 35.8 | 1.0229 | 34.8 | 32.7 | 30.7 | Dommbasstisch |  |
| 91 | Ibanez | BTB200 | 35.0 | HB: Bridge Coil 2 | 38.6 | 1.1029 | 37.5 | 35.3 | 33.1 | Dommbasstisch |  |
| 92 | Squier | Vintage Modified Precision Bass TB | 34.0 | HB: E/A | 19.4 | 0.5706 | 19.4 | 18.3 | 17.1 |  | Apparently a split-coil in a fat humbucker housing. |
| 92 | Squier | Vintage Modified Precision Bass TB | 34.0 | HB: D/G | 21.8 | 0.6412 | 21.8 | 20.5 | 19.2 |  | Apparently a split-coil in a fat humbucker housing. |
| 93 | G&L | SB-1 (1982) | 34.0 | Singlecoil (Splitcoil?) | 29.4 | 0.8647 | 29.4 | 27.7 | 25.9 | matteagle |  |
| 94 | Guild | Newark St. Starfire I (2013) | 30.75 | Singlecoil | 31.1 | 1.0114 | 34.4 | 32.4 | 30.3 |  | Bisonic Single-Coil |
| 95 | Fender | Bullet B30 | 30.0 | Splitcoil: E/A | 26.5 | 0.8833 | 30.0 | 28.3 | 26.5 | matt-o- |  |
| 95 | Fender | Bullet B30 | 30.0 | Splitcoil: D/G | 28.5 | 0.9500 | 32.3 | 30.4 | 28.5 | matt-o- |  |
| 96 | Squier | Bronco | 30.0 | Singlecoil | 23.9 | 0.7967 | 27.1 | 25.5 | 23.9 | matt-o- |  |
| 97 | Epiphone | Jack Casady | 34.0 | Middle of humbucker | 29.0 | 0.8529 | 29.0 | 27.3 | 25.6 | DaniG3 | Polepieces not visible. Breadth of pickup: 3cm |
| 98 | Modulus | Flea | 34.0 | Middle of humbucker | 33.8 | 0.9941 | 33.8 | 31.8 | 29.8 | DaniG3 | Polepieces not visible. Breadth of pickup: 4.8cm |
| 99 | Yamaha | Attitude 3 | 34.0 | Neck HB; Coil 1 | 21.6 | 0.6353 | 21.6 | 20.3 | 19.1 | DaniG3 |  |
| 99 | Yamaha | Attitude 3 | 34.0 | Neck HB; Coil 2 | 23.3 | 0.6853 | 23.3 | 21.9 | 20.6 | DaniG3 |  |
| 99 | Yamaha | Attitude 3 | 34.0 | P: E/A | 28.3 | 0.8324 | 28.3 | 26.6 | 25.0 | DaniG3 |  |
| 99 | Yamaha | Attitude 3 | 34.0 | P: D/G | 31.0 | 0.9118 | 31.0 | 29.2 | 27.4 | DaniG3 |  |
| 100 | Warwick | Streamer (1984) | 34.0 | P: E/A | 32.7 | 0.9618 | 32.7 | 30.8 | 28.9 | xsteps | reverse P |
| 100 | Warwick | Streamer (1984) | 34.0 | P: D/G | 29.9 | 0.8794 | 29.9 | 28.1 | 26.4 | xsteps | reverse P |
| 100 | Warwick | Streamer (1984) | 34.0 | J: Bridge | 38.3 | 1.1265 | 38.3 | 36.0 | 33.8 | xsteps |  |
| 101 | Gibson | Thunderbird (1996) | 34.0 | HB: Neck | 25.9 | 0.7618 | 25.9 | 24.4 | 22.9 | MauMau | Only the middle of the humbucker was measured |
| 101 | Gibson | Thunderbird (1996) | 34.0 | HB: Bridge | 36.9 | 1.0853 | 36.9 | 34.7 | 32.6 | MauMau | Only the middle of the humbucker was measured |
| 102 | Spector | Euro LX5 | 35.0 | HB: Neck | 31.4 | 0.8971 | 30.5 | 28.7 | 26.9 | MauMau | Only the middle of the humbucker was measured |
| 102 | Spector | Euro LX5 | 35.0 | HB: Bridge | 38.3 | 1.0943 | 37.2 | 35.0 | 32.8 | MauMau | Only the middle of the humbucker was measured |
| 103 | Self-Built | Leland Sklar Frankenstein | 34.0 | Neck: P E/A | 24.7 | 0.7265 | 24.7 | 23.2 | 21.8 | KTi01 | measurements were revers engineered from various pictures. reverse P |
| 103 | Self-Built | Leland Sklar Frankenstein | 34.0 | Neck: P D/G | 21.7 | 0.6382 | 21.7 | 20.4 | 19.1 | KTi01 | measurements were revers engineered from various pictures. reverse P |
| 103 | Self-Built | Leland Sklar Frankenstein | 34.0 | Bridge: P E/A | 36.9 | 1.0853 | 36.9 | 34.7 | 32.6 | KTi01 | measurements were revers engineered from various pictures. reverse P |
| 103 | Self-Built | Leland Sklar Frankenstein | 34.0 | Bridge: P D/G | 34.1 | 1.0029 | 34.1 | 32.1 | 30.1 | KTi01 | measurements were revers engineered from various pictures. reverse P |
| 104 | Danelectro | Longhorn | 29.75 | Neck @ G-String | 24.3 | 0.8168 | 27.8 | 26.1 | 24.5 | Mad Jazz Morales | the pickups are installed at an angle |
| 104 | Danelectro | Longhorn | 29.75 | Neck @ E-String | 23.8 | 0.8000 | 27.2 | 25.6 | 24.0 | Mad Jazz Morales | the pickups are installed at an angle |
| 104 | Danelectro | Longhorn | 29.75 | Bridge @ G-String | 34.3 | 1.1529 | 39.2 | 36.9 | 34.6 | Mad Jazz Morales | the pickups are installed at an angle |
| 104 | Danelectro | Longhorn | 29.75 | Bridge @ E-String | 33.8 | 1.1361 | 38.6 | 36.4 | 34.1 | Mad Jazz Morales | the pickups are installed at an angle |
| 105 | Sire | U5 Shortscale | 30.0 | P: E/A | 23.2 | 0.7733 | 26.3 | 24.7 | 23.2 | triple-U |  |
| 105 | Sire | U5 Shortscale | 30.0 | P: D/G | 25.9 | 0.8633 | 29.4 | 27.6 | 25.9 | triple-U |  |
| 105 | Sire | U5 Shortscale | 30.0 | J: Bridge | 34.7 | 1.1567 | 39.3 | 37.0 | 34.7 | triple-U |  |
| 106 | Sadowsky | MetroExpress Jazz Bass 21 Vintage (Gen. 1) | 34.0 | J: Neck | 28.5 | 0.8382 | 28.5 | 26.8 | 25.1 | xsteps |  |
| 106 | Sadowsky | MetroExpress Jazz Bass 21 Vintage (Gen. 1) | 34.0 | J: Bridge | 37.5 | 1.1029 | 37.5 | 35.3 | 33.1 | xsteps |  |
| 107 | MusicMan | Stingray SS4 Shortscale | 30.0 | MM | 303.0 | 10.1000 | 343.4 | 323.2 | 303.0 | triple-U | Only the middle of the humbucker was measured |
| 108 | Fender | Cabronita Precision | 34.0 | Splitcoil: E/A | 30.9 | 0.9088 | 30.9 | 29.1 | 27.3 | basslife | Replacement Pickup: TV Jones Thundermag |
| 108 | Fender | Cabronita Precision | 34.0 | Splitcoil: D/G | 29.2 | 0.8588 | 29.2 | 27.5 | 25.8 | basslife | Replacement Pickup: TV Jones Thundermag |
| 109 | Rickenbacker | 4003S5 Fireglo | 33.25 | Neck | 21.2 | 0.6376 | 21.7 | 20.4 | 19.1 | McValve |  |
| 109 | Rickenbacker | 4003S5 Fireglo | 33.25 | Bridge | 34.5 | 1.0376 | 35.3 | 33.2 | 31.1 | McValve |  |
| 110 | Hohner | B2 (1989) | 34.0 | Neck | 28.2 | 0.8294 | 28.2 | 26.5 | 24.9 | Scriptura | Only the middle of the humbucker was measured |
| 110 | Hohner | B2 (1989) | 34.0 | Bridge | 38.6 | 1.1353 | 38.6 | 36.3 | 34.1 | Scriptura | Only the middle of the humbucker was measured |
| 111 | MusicMan | Stingray 3EQ Fretless (1991) | 34.0 | MM | 34.5 | 1.0147 | 34.5 | 32.5 | 30.4 | Scriptura | Only the middle of the humbucker was measured |
| 112 | Human Base | Jonas (2004) | 34.0 | HB Neck | 29.5 | 0.8676 | 29.5 | 27.8 | 26.0 | Scriptura | Only the middle of the humbucker was measured |
| 112 | Human Base | Jonas (2004) | 34.0 | HB Bridge | 38.0 | 1.1176 | 38.0 | 35.8 | 33.5 | Scriptura | Only the middle of the humbucker was measured |
| 113 | Warwick | RB Starbass | 32.0 | J: Neck | 23.6 | 0.7375 | 25.1 | 23.6 | 22.1 | triple-U |  |
| 113 | Warwick | RB Starbass | 32.0 | J: Bridge | 34.6 | 1.0812 | 36.8 | 34.6 | 32.4 | triple-U |  |
| 114 | Ibanez | Musician MC924 (1982) | 34.0 | P: E/A | 26.9 | 0.7912 | 26.9 | 25.3 | 23.7 | stoneface |  |
| 114 | Ibanez | Musician MC924 (1982) | 34.0 | P: D/G | 29.9 | 0.8794 | 29.9 | 28.1 | 26.4 | stoneface |  |
| 114 | Ibanez | Musician MC924 (1982) | 34.0 | J: Bridge | 38.7 | 1.1382 | 38.7 | 36.4 | 34.1 | stoneface |  |
| 115 | Johnson | Stingray Copy | 34.0 | HB, Coil 1 | 34.0 | 1.0000 | 34.0 | 32.0 | 30.0 | Axel | the humbucker is thinner than a MM pickup |
| 115 | Johnson | Stingray Copy | 34.0 | HB, Coil 2 | 35.7 | 1.0500 | 35.7 | 33.6 | 31.5 | Axel | the humbucker is thinner than a MM pickup |
| 116 | Hartwood | Satelite (2025) | 32.0 | P: E/A | 20.2 | 0.6312 | 21.5 | 20.2 | 18.9 | wasabi 2.0 |  |
| 116 | Hartwood | Satelite (2025) | 32.0 | P: D/G | 23.0 | 0.7188 | 24.4 | 23.0 | 21.6 | wasabi 2.0 |  |

{{< /table >}}
