# Unit conversion

`get_conv_factor()` extracts a conversion factor for a pair of units.
Fails with error if units are not supported/convertible.

`get_conv_factors_all()` returns all conversion factors supported.

**Note:** Conversion factors come from unit definitions as per CDISC
standards.

## Usage

``` r
get_conv_factor(from_unit, to_unit)

get_conv_factors_all()
```
