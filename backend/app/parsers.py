from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Iterable, List

try:  # pragma: no cover - fallback para entornos sin dependencias instaladas
    from . import schemas  # type: ignore
except Exception:  # pragma: no cover - ejecutado en entorno de pruebas sin pydantic
    schemas = None  # type: ignore


CURRENCY_PATTERN = re.compile(r"^[€$]?\d+[\.,]?\d*[€$]?$")
QUANTITY_UNIT_PATTERN = re.compile(
    r"(?P<quantity>\d+[\.,]?\d*)\s*(?P<unit>kg|g|l|ml|unidad(?:es)?|uds?|u|pack|pz|pcs)?",
    flags=re.IGNORECASE,
)
MULTIPLIER_PATTERN = re.compile(r"(?P<count>\d+)[xX](?P<quantity>\d+[\.,]?\d*)(?P<unit>[a-zA-Z]+)?")
TRAILING_MULTIPLIER_PATTERN = re.compile(r"^[xX](?P<count>\d+)$")


@dataclass
class ParsedLine:
    name: str
    quantity: float
    unit: str


@dataclass
class ReceiptItemPayload:
    product_name: str
    quantity: float
    unit: str
    confidence: float | None = None


@dataclass
class ReceiptParsePayload:
    raw_text: str
    items: List[ReceiptItemPayload]
    warnings: List[str]


@dataclass
class InventoryItemCreatePayload:
    product_name: str
    quantity: float
    unit: str
    expires_at: None = None
    notes: None = None


UNIT_NORMALIZATION = {
    "uds": "unidad",
    "ud": "unidad",
    "u": "unidad",
    "unidad": "unidad",
    "unidades": "unidad",
    "pz": "unidad",
    "pcs": "unidad",
    "pack": "unidad",
    "kg": "kg",
    "g": "g",
    "l": "l",
    "ml": "ml",
}


def _clean_line(line: str) -> str:
    return line.strip()


def _remove_price_tokens(tokens: List[str]) -> List[str]:
    cleaned = list(tokens)
    while cleaned and CURRENCY_PATTERN.match(cleaned[-1]):
        cleaned.pop()
    return cleaned


def _normalize_unit(unit: str | None) -> str:
    if not unit:
        return "unidad"
    return UNIT_NORMALIZATION.get(unit.lower(), unit.lower())


def _parse_multiplier(tokens: List[str]) -> tuple[float | None, str | None]:
    joined = "".join(tokens)
    match = MULTIPLIER_PATTERN.search(joined)
    if match:
        count = float(match.group("count"))
        quantity = float(match.group("quantity").replace(",", "."))
        unit = match.group("unit")
        total_quantity = count * quantity
        return total_quantity, unit

    for index, token in enumerate(tokens):
        trailing = TRAILING_MULTIPLIER_PATTERN.match(token)
        if trailing and index > 0:
            prev_segment = tokens[index - 1]
            prev_match = QUANTITY_UNIT_PATTERN.search(prev_segment)
            if prev_match:
                base_quantity = float(prev_match.group("quantity").replace(",", "."))
                unit = prev_match.group("unit")
                total_quantity = base_quantity * float(trailing.group("count"))
                return total_quantity, unit
    return None, None


def parse_line(line: str) -> ParsedLine | None:
    cleaned_line = _clean_line(line)
    if not cleaned_line:
        return None

    tokens = _remove_price_tokens(cleaned_line.split())
    if not tokens:
        return None

    multiplier_quantity, multiplier_unit = _parse_multiplier(tokens)

    quantity = None
    unit = None
    match = None
    for match in reversed(list(QUANTITY_UNIT_PATTERN.finditer(" ".join(tokens)))):
        quantity = float(match.group("quantity").replace(",", "."))
        unit = match.group("unit")
        break

    if quantity is None:
        quantity = 1.0
    if unit is None and multiplier_unit:
        unit = multiplier_unit
    normalized_unit = _normalize_unit(unit)

    name_tokens: List[str]
    if match:
        name_tokens = " ".join(tokens)[: match.start()].split()
    else:
        name_tokens = tokens

    name = " ".join(name_tokens).strip()
    if not name:
        name = "Artículo"

    if multiplier_quantity is not None:
        quantity = multiplier_quantity
        if multiplier_unit:
            normalized_unit = _normalize_unit(multiplier_unit)

    return ParsedLine(name=name, quantity=quantity, unit=normalized_unit)


def parse_receipt_text(text: str) -> schemas.ReceiptParseResponse:
    raw_items: List[ReceiptItemPayload] = []
    warnings: List[str] = []

    if not text.strip():
        warnings.append("El ticket está vacío o no se pudo extraer texto.")
        if schemas:
            return schemas.ReceiptParseResponse(raw_text=text, items=[], warnings=warnings)
        return ReceiptParsePayload(raw_text=text, items=[], warnings=warnings)

    for raw_line in text.splitlines():
        line = raw_line.strip()
        if not line:
            continue
        if line.lower().startswith(("total", "subtotal", "iva")):
            continue
        parsed = parse_line(line)
        if not parsed:
            warnings.append(f"No se pudo interpretar la línea: '{line}'")
            continue
        raw_items.append(
            ReceiptItemPayload(
                product_name=parsed.name,
                quantity=parsed.quantity,
                unit=parsed.unit,
            )
        )

    if not raw_items:
        warnings.append("No se encontraron artículos en el ticket.")

    if schemas:
        schema_items = [
            schemas.ReceiptItem(
                product_name=item.product_name,
                quantity=item.quantity,
                unit=item.unit,
            )
            for item in raw_items
        ]
        return schemas.ReceiptParseResponse(raw_text=text, items=schema_items, warnings=warnings)

    return ReceiptParsePayload(raw_text=text, items=raw_items, warnings=warnings)


def transform_receipt_items_to_inventory(
    items: Iterable[schemas.ReceiptItem],
) -> List[schemas.InventoryItemCreate]:
    inventory_entries = []
    for item in items:
        payload = {
            "product_name": getattr(item, "product_name"),
            "quantity": getattr(item, "quantity"),
            "unit": getattr(item, "unit"),
        }
        if schemas:
            entry = schemas.InventoryItemCreate(**payload)
        else:  # pragma: no cover - usado en entorno de pruebas sin dependencias
            entry = InventoryItemCreatePayload(**payload)  # type: ignore[assignment]
        inventory_entries.append(entry)  # type: ignore[arg-type]
    return inventory_entries
