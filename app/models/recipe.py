from __future__ import annotations

from typing import List, Optional
from uuid import UUID, uuid4

from pydantic import BaseModel, Field


class RecipeIngredient(BaseModel):
    name: str = Field(..., min_length=1)
    quantity: float = Field(..., gt=0)
    unit: str = Field(..., min_length=1)


class RecipeBase(BaseModel):
    name: str = Field(..., min_length=1)
    description: Optional[str] = None
    ingredients: List[RecipeIngredient] = Field(default_factory=list)


class RecipeCreate(RecipeBase):
    pass


class RecipeUpdate(BaseModel):
    name: Optional[str] = Field(None, min_length=1)
    description: Optional[str] = None
    ingredients: Optional[List[RecipeIngredient]] = None


class Recipe(RecipeBase):
    id: UUID = Field(default_factory=uuid4)

    class Config:
        orm_mode = True
