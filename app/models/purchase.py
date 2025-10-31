from __future__ import annotations

from datetime import datetime
from typing import Optional
from uuid import UUID, uuid4

from pydantic import BaseModel, Field


class PurchaseBase(BaseModel):
    inventory_item_id: UUID
    quantity: float = Field(..., gt=0)
    purchased_at: datetime = Field(default_factory=datetime.utcnow)
    notes: Optional[str] = None


class PurchaseCreate(PurchaseBase):
    pass


class Purchase(PurchaseBase):
    id: UUID = Field(default_factory=uuid4)

    class Config:
        orm_mode = True
