"""Roman numerals.

See study.LICENSE for copyright and license information.
"""
NUMERALS = "IV", "XL", "CD"

def toRoman(num):
    text, layer = "", len(NUMERALS)
    scale, unit = 1000, "M"
    while num and layer > 0:
        q, num = divmod(num, scale)
        text += q * unit
        scale /= 10
        layer -= 1
        cut, five = NUMERALS[layer]
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

# Could perhaps be more permissive: IC, IL, XD, XM &c. and even IIXX, VL, VC.
def fromRoman(text):
    text = text.upper()
    num, layer = 0, len(NUMERALS)
    scale, unit = 1000, "M"
    while text and layer > 0:
        while text.startswith(unit):
            text = text[1:]
            num += scale
        scale /= 10
        layer -= 1
        cut, five = NUMERALS[layer]
        if text.startswith(cut + unit):
            text = text[2:]
            num += 9 * scale
        elif text.startswith(cut + five):
            text = text[2:]
            num += 4 * scale
        while text.startswith(five):
            text = text[1:]
            num += 5 * scale
        unit = cut
    if any(ch != unit for ch in text):
        raise ValueError("Malformed numeral: " + text)
    return num + len(text)
