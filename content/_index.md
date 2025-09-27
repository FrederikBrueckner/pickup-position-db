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

{{< /table >}}
