from app import parsers


def test_parse_receipt_text_with_basic_lines():
    raw = """
    LECHE ENTERA 1L x2 2,40€
    TOMATE ENSALADA 0,75kg 1,80€
    HUEVOS CAMPEROS 12ud 3,20€
    TOTAL 7,40€
    """
    result = parsers.parse_receipt_text(raw)
    assert len(result.items) == 3

    milk = result.items[0]
    assert milk.product_name.lower().startswith("leche")
    assert milk.quantity == 2.0
    assert milk.unit == "l"

    tomato = result.items[1]
    assert tomato.quantity == 0.75
    assert tomato.unit == "kg"

    eggs = result.items[2]
    assert eggs.quantity == 12
    assert eggs.unit == "unidad"


def test_parse_receipt_text_handles_empty_lines():
    result = parsers.parse_receipt_text("\n\n")
    assert result.items == []
    assert any("vacío" in warning for warning in result.warnings)


def test_transform_receipt_items_to_inventory():
    parsed = parsers.parse_receipt_text("Pan integral 1 ud 1,20€")
    entries = parsers.transform_receipt_items_to_inventory(parsed.items)
    assert len(entries) == 1
    entry = entries[0]
    assert entry.product_name == "Pan integral"
    assert entry.quantity == 1
    assert entry.unit == "unidad"
