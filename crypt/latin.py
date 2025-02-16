"""Roman numerals.

See study.LICENSE for copyright and license information.
"""

def toRoman(num):
    numerals = "IV", "XL", "CD"
    layer = len(numerals)
    scale, unit = 1000, "M"
    text = ""
    while num and layer > 0:
        q, num = divmod(num, scale)
        text += q * unit
        scale /= 10
        layer -= 1
        cut, five = numerals[layer]
        if num >= 9 * scale:
            text += cut + unit
            num -= 9 * scale
        elif num >= 5 * scale:
            text += five
            num -= 5 * scale
        elif num >= 4 * scale:
            text += cut + five
            num -= 4 * scale
        unit = cut
    return text + num * unit
