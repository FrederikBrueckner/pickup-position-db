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

{{< table sortable="true" searchable="true" >}}
|  #  | Brand | Make | Scale (inch) | Pickup/Coil | Measurement (cm) | Normalized | Target 34 (cm) | Target 32 (cm) | Target 30 (cm) | Reporter | Comment |
| --- | ----- | ---- | ------------ | ----------- | ---------------- | ---------- | -------------- | -------------- | -------------- | -------- | ------- |
| 1 | Fender | CJJ Jazzbass | 34.0 | J:Neck | 27.5 | 0.8088 | 27.5 | 25.9 | 24.3 | Doschd | TODO: CJJ or CCJ? |
| 1 | Fender | CJJ Jazzbass | 34.0 | J:Bridge | 36.5 | 1.0735 | 36.5 | 34.4 | 32.2 | Doschd | TODO: CJJ or CCJ? |
| 2 | Fenix | Precision | 34.0 | P: E-A | 28.3 | 0.8324 | 28.3 | 26.6 | 25.0 | mosys |  |
| 2 | Fenix | Precision | 34.0 | P: D-G | 31.0 | 0.9118 | 31.0 | 29.2 | 27.4 | mosys |  |
| 3 | Hartwood | Satelite | 32.0 | P: E-A | 20.2 | 0.6312 | 21.5 | 20.2 | 18.9 | wasabi 2.0 | was reported as "mid scale". 32 inch were assumed |
| 3 | Hartwood | Satelite | 32.0 | P: D-G | 23.0 | 0.7188 | 24.4 | 23.0 | 21.6 | wasabi 2.0 | was reported as "mid scale". 32 inch were assumed |
| 4 | Sterling | SUB Ray 4 | 34.0 | MM: Neck Coil | 33.3 | 0.9794 | 33.3 | 31.3 | 29.4 | Seven Bass |  |
| 4 | Sterling | SUB Ray 4 | 34.0 | MM: Bridge Coil | 35.8 | 1.0529 | 35.8 | 33.7 | 31.6 | Seven Bass |  |
| 5 | Guild | Newark St. Starfire | 30.75 | Singlecoil | 31.1 | 1.0114 | 34.4 | 32.4 | 30.3 | RomanS | Bisonic Singlecoil |
| 6 | Rickenbacker | 4003 / V63 | 33.25 | Neck | 20.7 | 0.6226 | 21.2 | 19.9 | 18.7 | Oli Wan | Test: per Bass-Comment. Test: per Measurement Comment |
| 6 | Rickenbacker | 4003 / V63 | 33.25 | Bridge | 32.0 | 0.9624 | 32.7 | 30.8 | 28.9 | Oli Wan | Test: per Bass-Comment |

{{< /table >}}
