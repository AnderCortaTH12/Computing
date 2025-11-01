from __future__ import annotations

from datetime import date
from typing import List, Optional
from uuid import UUID

from pydantic import BaseModel, Field, validator


class InventoryItemBase(BaseModel):
    product_name: str = Field(..., description="Nombre del producto")
    quantity: float = Field(..., gt=0, description="Cantidad del producto a registrar")
    unit: str = Field(default="unidad", description="Unidad de medida (kg, l, unidad, etc.)")
    expires_at: Optional[date] = Field(default=None)
    notes: Optional[str] = None

    @validator("unit")
    def normalize_unit(cls, value: str) -> str:
        normalized = value.strip().lower()
        if normalized in {"uds", "ud", "unidad", "unidades", "u"}:
            return "unidad"
        if normalized in {"kg", "kilogramo", "kilogramos"}:
            return "kg"
        if normalized in {"g", "gramo", "gramos"}:
            return "g"
        if normalized in {"l", "litro", "litros"}:
            return "l"
        if normalized in {"ml", "mililitro", "mililitros"}:
            return "ml"
        return normalized


class InventoryItemCreate(InventoryItemBase):
    pass


class InventoryItemUpdate(BaseModel):
    quantity: Optional[float] = Field(default=None, gt=0)
    expires_at: Optional[date] = None
    notes: Optional[str] = None


class InventoryItemRead(BaseModel):
    id: UUID
    product_id: UUID
    product_name: str
    quantity: float
    unit: str
    expires_at: Optional[date]
    notes: Optional[str]


class InventoryProductSummary(BaseModel):
    product_id: UUID
    product_name: str
    unit: str
    total_quantity: float
    reorder_threshold: Optional[float]
    items: List[InventoryItemRead]


class ReceiptItem(BaseModel):
    product_name: str
    quantity: float
    unit: str
    confidence: Optional[float] = None


class ReceiptParseResponse(BaseModel):
    raw_text: str
    items: List[ReceiptItem]
    warnings: List[str] = Field(default_factory=list)


class ReceiptCommitRequest(BaseModel):
    items: List[InventoryItemCreate]


class MissingIngredient(BaseModel):
    product_name: str
    required_quantity: float
    available_quantity: float
    unit: str


class RecipeRead(BaseModel):
    id: UUID
    name: str
    description: Optional[str]
    servings: int
    instructions: Optional[str]
    can_make: bool
    missing_ingredients: List[MissingIngredient]
