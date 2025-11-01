from __future__ import annotations

from datetime import date
from typing import List, Optional
from uuid import UUID, uuid4

from sqlmodel import Field, Relationship, SQLModel


class Product(SQLModel, table=True):
    __tablename__ = "products"

    id: UUID = Field(default_factory=uuid4, primary_key=True, index=True)
    name: str = Field(index=True)
    category: Optional[str] = None
    unit: str = Field(default="unidad")
    reorder_threshold: Optional[float] = Field(default=None, nullable=True)

    inventory_items: List["InventoryItem"] = Relationship(back_populates="product")
    recipe_ingredients: List["RecipeIngredient"] = Relationship(back_populates="product")


class InventoryItem(SQLModel, table=True):
    __tablename__ = "inventory_items"

    id: UUID = Field(default_factory=uuid4, primary_key=True, index=True)
    product_id: UUID = Field(foreign_key="products.id", index=True)
    quantity: float = Field(default=0)
    storage_location: Optional[str] = Field(default=None, nullable=True)
    batch_code: Optional[str] = Field(default=None, nullable=True)
    expires_at: Optional[date] = Field(default=None, nullable=True)
    notes: Optional[str] = Field(default=None, nullable=True)

    product: Optional[Product] = Relationship(back_populates="inventory_items")


class Recipe(SQLModel, table=True):
    __tablename__ = "recipes"

    id: UUID = Field(default_factory=uuid4, primary_key=True, index=True)
    name: str = Field(index=True)
    description: Optional[str] = None
    servings: int = Field(default=2)
    instructions: Optional[str] = None

    ingredients: List["RecipeIngredient"] = Relationship(back_populates="recipe")


class RecipeIngredient(SQLModel, table=True):
    __tablename__ = "recipe_ingredients"

    id: UUID = Field(default_factory=uuid4, primary_key=True, index=True)
    recipe_id: UUID = Field(foreign_key="recipes.id", index=True)
    product_id: UUID = Field(foreign_key="products.id", index=True)
    quantity_required: float = Field(default=0)

    recipe: Optional[Recipe] = Relationship(back_populates="ingredients")
    product: Optional[Product] = Relationship(back_populates="recipe_ingredients")
"""SQLAlchemy models for the nutrition profile domain."""
from __future__ import annotations

from datetime import datetime

from sqlalchemy import Column, DateTime, Float, Integer, Text

from .database import Base


class UserProfile(Base):
    """Store anthropometric data and preferences for a user."""

    __tablename__ = "user_profiles"

    id = Column(Integer, primary_key=True, index=True)
    weight = Column(Float, nullable=True)
    height = Column(Float, nullable=True)
    goals = Column(Text, nullable=True)
    dietary_restrictions = Column(Text, nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow, nullable=False)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow, nullable=False)

    def update_from_dict(self, data: dict[str, object]) -> None:
        """Apply updates using a dict produced by a Pydantic model."""
        for key, value in data.items():
            setattr(self, key, value)
