---
title: Pickup Position DB
modules: ["simple-datatables"]
---

A modest searchable and sortable list of Pickup measurements. Mostly compiled by the good people of [bassic.de](https://www.bassic.de/threads/pu-positions-database.14789156/).

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
| 5 | Fender | Jazzbass Fretless | 34.0 | J: Neck | 27.7 | 0.8147 | 27.7 | 26.1 | 24.4 | EMUBASS |  |
| 5 | Fender | Jazzbass Fretless | 34.0 | J: Bridge | 36.8 | 1.0824 | 36.8 | 34.6 | 32.5 | EMUBASS |  |
| 6 | Fender | Fender JB Special Edition 70s Spacing | 34.0 | J: Neck | 27.7 | 0.8147 | 27.7 | 26.1 | 24.4 | BassManni |  |
| 6 | Fender | Fender JB Special Edition 70s Spacing | 34.0 | J: Bridge | 37.7 | 1.1088 | 37.7 | 35.5 | 33.3 | BassManni |  |
| 7 | Johnson | Jazzbass | 34.0 | J: Neck | 27.8 | 0.8176 | 27.8 | 26.2 | 24.5 | BigB |  |
| 7 | Johnson | Jazzbass | 34.0 | J: Bridge | 38.0 | 1.1176 | 38.0 | 35.8 | 33.5 | BigB |  |
| 8 | Warwick | Streamer Stage 2 (2 J-Singlecoils) | 34.0 | J: Neck | 31.4 | 0.9235 | 31.4 | 29.6 | 27.7 | jam_bass |  |
| 8 | Warwick | Streamer Stage 2 (2 J-Singlecoils) | 34.0 | J: Bridge | 38.5 | 1.1324 | 38.5 | 36.2 | 34.0 | jam_bass |  |
| 9 | Fender | Jaguar Deluxe MIJ | 34.0 | J: Neck | 27.4 | 0.8059 | 27.4 | 25.8 | 24.2 | stoneface |  |
| 9 | Fender | Jaguar Deluxe MIJ | 34.0 | J: Bridge | 38.9 | 1.1441 | 38.9 | 36.6 | 34.3 | stoneface |  |
| 10 | Fenix | Jazzbass | 34.0 | J: Neck | 27.6 | 0.8118 | 27.6 | 26.0 | 24.4 | stoneface |  |
| 10 | Fenix | Jazzbass | 34.0 | J: Bridge | 36.7 | 1.0794 | 36.7 | 34.5 | 32.4 | stoneface |  |
| 11 | Guilber | ??? | 34.0 | J: Neck | 26.9 | 0.7912 | 26.9 | 25.3 | 23.7 | stoneface |  |
| 11 | Guilber | ??? | 34.0 | J: Bridge | 36.5 | 1.0735 | 36.5 | 34.4 | 32.2 | stoneface |  |
| 12 | Fender | Victor Nailey JB | 34.0 | J: Neck | 27.5 | 0.8088 | 27.5 | 25.9 | 24.3 | olebass |  |
| 12 | Fender | Victor Nailey JB | 34.0 | J: Bridge | 36.5 | 1.0735 | 36.5 | 34.4 | 32.2 | olebass |  |
| 13 | Lodestone | Primal Artist V JB | 34.0 | J: Neck | 28.0 | 0.8235 | 28.0 | 26.4 | 24.7 | olebass |  |
| 13 | Lodestone | Primal Artist V JB | 34.0 | J: Bridge | 37.0 | 1.0882 | 37.0 | 34.8 | 32.6 | olebass |  |
| 14 | Sandberg | California JM V | 34.0 | J: Neck | 28.0 | 0.8235 | 28.0 | 26.4 | 24.7 | olebass |  |
| 14 | Sandberg | California JM V | 34.0 | J: Bridge | 36.9 | 1.0853 | 36.9 | 34.7 | 32.6 | olebass |  |
| 15 | Vester | Clipper IV | 34.0 | J: Neck | 30.0 | 0.8824 | 30.0 | 28.2 | 26.5 | olebass |  |
| 15 | Vester | Clipper IV | 34.0 | J: Bridge | 37.5 | 1.1029 | 37.5 | 35.3 | 33.1 | olebass |  |
| 16 | Yamaha | BB Standard | 34.0 | J: Neck | 27.2 | 0.8 | 27.2 | 25.6 | 24.0 | Youth |  |
| 16 | Yamaha | BB Standard | 34.0 | J: Bridge | 36.4 | 1.0706 | 36.4 | 34.3 | 32.1 | Youth |  |
| 17 | Yamaha | BB604 | 34.0 | J: Neck | 30.5 | 0.8971 | 30.5 | 28.7 | 26.9 | Youth |  |
| 17 | Yamaha | BB604 | 34.0 | J: Bridge | 38.2 | 1.1235 | 38.2 | 36.0 | 33.7 | Youth |  |
| 18 | Fender | Jaguar MIJ | 34.0 | J: Neck | 27.3 | 0.8029 | 27.3 | 25.7 | 24.1 | Youth |  |
| 18 | Fender | Jaguar MIJ | 34.0 | J: Bridge | 37.7 | 1.1088 | 37.7 | 35.5 | 33.3 | Youth |  |
| 19 | BolleBass | Corinna | 32.67 | J: Neck | 27.9 | 0.854 | 29.0 | 27.3 | 25.6 | bassilisk |  |
| 19 | BolleBass | Corinna | 32.67 | J: Bridge | 36.9 | 1.1295 | 38.4 | 36.1 | 33.9 | bassilisk |  |
| 20 | Sandberg | TT | 34.0 | J: Neck | 27.7 | 0.8147 | 27.7 | 26.1 | 24.4 | Seven Basses |  |
| 20 | Sandberg | TT | 34.0 | J: Bridge | 36.7 | 1.0794 | 36.7 | 34.5 | 32.4 | Seven Basses |  |
| 21 | Marleaux | Votan X-Tra | 34.0 | J: Neck | 27.5 | 0.8088 | 27.5 | 25.9 | 24.3 | Seven Basses |  |
| 21 | Marleaux | Votan X-Tra | 34.0 | J: Bridge | 36.5 | 1.0735 | 36.5 | 34.4 | 32.2 | Seven Basses |  |
| 22 | Squier | Affinity Precision - 20th Anniversary | 34.0 | P: E/A | 28.1 | 0.8265 | 28.1 | 26.4 | 24.8 | Axel |  |
| 22 | Squier | Affinity Precision - 20th Anniversary | 34.0 | P: D/G | 30.9 | 0.9088 | 30.9 | 29.1 | 27.3 | Axel |  |
| 23 | Self-Built | Precision | 34.0 | P: E/A | 28.1 | 0.8265 | 28.1 | 26.4 | 24.8 | stoneface |  |
| 23 | Self-Built | Precision | 34.0 | P: D/G | 30.9 | 0.9088 | 30.9 | 29.1 | 27.3 | stoneface |  |
| 24 | Fender | Precision Road Worn | 34.0 | P: E/A | 28.4 | 0.8353 | 28.4 | 26.7 | 25.1 | indian66 |  |
| 24 | Fender | Precision Road Worn | 34.0 | P: D/G | 31.1 | 0.9147 | 31.1 | 29.3 | 27.4 | indian66 |  |
| 25 | Fender | Precision | 34.0 | P: E/A | 28.3 | 0.8324 | 28.3 | 26.6 | 25.0 | Nick McNoise |  |
| 25 | Fender | Precision | 34.0 | P: D/G | 30.8 | 0.9059 | 30.8 | 29.0 | 27.2 | Nick McNoise |  |
| 26 | Fender | 51 Model CIJ (Singlecoil) | 34.0 | P Singlecoil | 26.2 | 0.7706 | 26.2 | 24.7 | 23.1 |  |  |
| 27 | Aria | Pro II RSB-Deluxe (Revers P) | 34.0 | P: E/A | 33.8 | 0.9941 | 33.8 | 31.8 | 29.8 | Youth |  |
| 27 | Aria | Pro II RSB-Deluxe (Revers P) | 34.0 | P: D/G | 32.1 | 0.9441 | 32.1 | 30.2 | 28.3 | Youth |  |
| 28 | Fenix | Precision | 34.0 | P: E/A | 28.3 | 0.8324 | 28.3 | 26.6 | 25.0 | mosys |  |
| 28 | Fenix | Precision | 34.0 | P: D/G | 31.0 | 0.9118 | 31.0 | 29.2 | 27.4 | mosys |  |
| 29 | Travis Bean | TB 2000 | 33.25 | SC: Neck | 18.9 | 0.5684 | 19.3 | 18.2 | 17.1 | quarkfrosch |  |
| 29 | Travis Bean | TB 2000 | 33.25 | SC: Bridge | 32.0 | 0.9624 | 32.7 | 30.8 | 28.9 | quarkfrosch |  |
| 30 | Kramer | XKB-20 | 33.75 | P: E/A | 27.4 | 0.8119 | 27.6 | 26.0 | 24.4 | quarkfrosch |  |
| 30 | Kramer | XKB-20 | 33.75 | P: D/G | 30.4 | 0.9007 | 30.6 | 28.8 | 27.0 | quarkfrosch |  |
| 31 | Vantage | VP-795B | 33.25 | P: Neck E/A | 22.3 | 0.6707 | 22.8 | 21.5 | 20.1 | quarkfrosch |  |
| 31 | Vantage | VP-795B | 33.25 | P: Neck D/G | 25.7 | 0.7729 | 26.3 | 24.7 | 23.2 | quarkfrosch |  |
| 31 | Vantage | VP-795B | 33.25 | P: Bridge E/A | 31.7 | 0.9534 | 32.4 | 30.5 | 28.6 | quarkfrosch |  |
| 31 | Vantage | VP-795B | 33.25 | P: Bridge D/G | 34.6 | 1.0406 | 35.4 | 33.3 | 31.2 | quarkfrosch |  |
| 32 | Warwick | Corvette Std | 34.0 | J: Neck | 29.8 | 0.8765 | 29.8 | 28.0 | 26.3 | quarkfrosch |  |
| 32 | Warwick | Corvette Std | 34.0 | 2J1: Bridge | 38.0 | 1.1176 | 38.0 | 35.8 | 33.5 | quarkfrosch |  |
| 32 | Warwick | Corvette Std | 34.0 | 2J2: Bridge | 40.4 | 1.1882 | 40.4 | 38.0 | 35.6 | quarkfrosch |  |
| 33 | Warwick | Fortress One | 34.0 | P: E/A | 30.2 | 0.8882 | 30.2 | 28.4 | 26.6 | quarkfrosch |  |
| 33 | Warwick | Fortress One | 34.0 | P: D/G | 33.0 | 0.9706 | 33.0 | 31.1 | 29.1 | quarkfrosch |  |
| 33 | Warwick | Fortress One | 34.0 | J: Bridge | 38.6 | 1.1353 | 38.6 | 36.3 | 34.1 | quarkfrosch |  |
| 34 | Charvel | 2B | 34.0 | P: E/A | 28.3 | 0.8324 | 28.3 | 26.6 | 25.0 | quarkfrosch |  |
| 34 | Charvel | 2B | 34.0 | P: D/G | 31.2 | 0.9176 | 31.2 | 29.4 | 27.5 | quarkfrosch |  |
| 34 | Charvel | 2B | 34.0 | J: Bridge | 37.7 | 1.1088 | 37.7 | 35.5 | 33.3 | quarkfrosch |  |
| 35 | Fenix | P-Bass | 34.0 | P: E/A | 28.2 | 0.8294 | 28.2 | 26.5 | 24.9 | quarkfrosch |  |
| 35 | Fenix | P-Bass | 34.0 | P: D/G | 31.1 | 0.9147 | 31.1 | 29.3 | 27.4 | quarkfrosch |  |
| 35 | Fenix | P-Bass | 34.0 | J: Bridge | 38.4 | 1.1294 | 38.4 | 36.1 | 33.9 | quarkfrosch |  |
| 36 | Fenix | Jazz-Bass | 34.0 | J: Neck | 27.5 | 0.8088 | 27.5 | 25.9 | 24.3 | quarkfrosch |  |
| 36 | Fenix | Jazz-Bass | 34.0 | J: Bridge | 36.7 | 1.0794 | 36.7 | 34.5 | 32.4 | quarkfrosch |  |
| 37 | Fenix | Jazz-Bass V | 34.0 | J: Neck | 27.5 | 0.8088 | 27.5 | 25.9 | 24.3 | quarkfrosch |  |
| 37 | Fenix | Jazz-Bass V | 34.0 | J: Bridge | 36.7 | 1.0794 | 36.7 | 34.5 | 32.4 | quarkfrosch |  |
| 38 | Squier | Jaguar VM | 30.0 | P: E/A | 22.8 | 0.76 | 25.8 | 24.3 | 22.8 | quarkfrosch |  |
| 38 | Squier | Jaguar VM | 30.0 | P: D/G | 25.8 | 0.86 | 29.2 | 27.5 | 25.8 | quarkfrosch |  |
| 38 | Squier | Jaguar VM | 30.0 | J: Bridge | 33.7 | 1.1233 | 38.2 | 35.9 | 33.7 | quarkfrosch |  |
| 39 | Mayones | Slogan Classic 4 | 34.25 | HB: Neck | 29.2 | 0.8526 | 29.0 | 27.3 | 25.6 | quarkfrosch |  |
| 39 | Mayones | Slogan Classic 4 | 34.25 | HB: Bridge | 38.0 | 1.1095 | 37.7 | 35.5 | 33.3 | quarkfrosch |  |
| 40 | OLP | Tony Levin 5 | 34.0 | MM1: Bridge | 33.9 | 0.9971 | 33.9 | 31.9 | 29.9 | quarkfrosch |  |
| 40 | OLP | Tony Levin 5 | 34.0 | MM2: Bridge | 39.2 | 1.1529 | 39.2 | 36.9 | 34.6 | quarkfrosch |  |
| 41 | Bass Collection | SB302 PWH | 34.25 | P: E/A | 30.1 | 0.8788 | 29.9 | 28.1 | 26.4 | quarkfrosch |  |
| 41 | Bass Collection | SB302 PWH | 34.25 | P: D/G | 33.2 | 0.9693 | 33.0 | 31.0 | 29.1 | quarkfrosch |  |
| 41 | Bass Collection | SB302 PWH | 34.25 | J: Bridge | 38.1 | 1.1124 | 37.8 | 35.6 | 33.4 | quarkfrosch |  |
| 42 | Wotan | Shark | 34.5 | P: Neck E/A | 21.0 | 0.6087 | 20.7 | 19.5 | 18.3 | quarkfrosch |  |
| 42 | Wotan | Shark | 34.5 | P: Neck D/G | 23.7 | 0.687 | 23.4 | 22.0 | 20.6 | quarkfrosch |  |
| 42 | Wotan | Shark | 34.5 | P: Bridge E/A | 34.2 | 0.9913 | 33.7 | 31.7 | 29.7 | quarkfrosch |  |
| 42 | Wotan | Shark | 34.5 | P: Bridge D/G | 37.7 | 1.0928 | 37.2 | 35.0 | 32.8 | quarkfrosch |  |
| 43 | Kramer | DMZ 5000 | 33.75 | P: Neck E/A | 27.5 | 0.8148 | 27.7 | 26.1 | 24.4 | quarkfrosch |  |
| 43 | Kramer | DMZ 5000 | 33.75 | P: Neck D/G | 30.3 | 0.8978 | 30.5 | 28.7 | 26.9 | quarkfrosch |  |
| 43 | Kramer | DMZ 5000 | 33.75 | J: Bridge | 39.0 | 1.1556 | 39.3 | 37.0 | 34.7 | quarkfrosch |  |
| 44 | Esh | Stinger I | 34.25 | HB: Neck | 32.1 | 0.9372 | 31.9 | 30.0 | 28.1 | quarkfrosch |  |
| 44 | Esh | Stinger I | 34.25 | HB: Bridge | 38.7 | 1.1299 | 38.4 | 36.2 | 33.9 | quarkfrosch |  |
| 45 | Esh | Stinger II - V | 34.25 | HB: Neck | 30.0 | 0.8759 | 29.8 | 28.0 | 26.3 | quarkfrosch |  |
| 45 | Esh | Stinger II - V | 34.25 | HB: Bridge | 37.8 | 1.1036 | 37.5 | 35.3 | 33.1 | quarkfrosch |  |

{{< /table >}}
