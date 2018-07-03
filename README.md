# Diamonds

This script was created in order to compare lab grown diamonds vs earth mined diamonds. I am a big believer in ethical business so it interested me to find out a bit more about how they compare.

## Methods

R fortunately comes with diamonds data built in by using `data(diamonds)`.

In order to add in lab grown diamond data, it was manually downloaded from [Brilliant Earth's database](https://www.brilliantearth.com/lab-diamonds-search/). This was done due to the strange format the data is presented (with the table only updating fully when manually scrolled on the page. Thus Excel was employed in order to process the data.

## Results

The following plot shows how earth mined diamonds compare to lab grown diamonds:
![Diamond data](https://raw.githubusercontent.com/cajpearce/diamonds/master/images/diamonds.png)

Note that lab grown diamonds are nearly universally cheaper.

## Shortfalls

It is difficult to ascertain if the pricing of these two datasets are actually comparable (whether by currency, taxes, or other charges).
