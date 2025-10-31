from __future__ import annotations

from typing import Dict, List, Optional
from uuid import UUID

from app.models.inventory import InventoryItem
from app.models.purchase import Purchase
from app.models.recipe import Recipe


class InMemoryStorage:
    """Simple in-memory storage for demo purposes."""

    def __init__(self) -> None:
        self._inventory: Dict[UUID, InventoryItem] = {}
        self._recipes: Dict[UUID, Recipe] = {}
        self._purchases: Dict[UUID, Purchase] = {}

    # Inventory operations
    def list_inventory(self) -> List[InventoryItem]:
        return list(self._inventory.values())

    def get_inventory(self, item_id: UUID) -> Optional[InventoryItem]:
        return self._inventory.get(item_id)

    def save_inventory(self, item: InventoryItem) -> InventoryItem:
        self._inventory[item.id] = item
        return item

    def delete_inventory(self, item_id: UUID) -> None:
        self._inventory.pop(item_id, None)

    # Recipe operations
    def list_recipes(self) -> List[Recipe]:
        return list(self._recipes.values())

    def get_recipe(self, recipe_id: UUID) -> Optional[Recipe]:
        return self._recipes.get(recipe_id)

    def save_recipe(self, recipe: Recipe) -> Recipe:
        self._recipes[recipe.id] = recipe
        return recipe

    def delete_recipe(self, recipe_id: UUID) -> None:
        self._recipes.pop(recipe_id, None)

    # Purchase operations
    def list_purchases(self) -> List[Purchase]:
        return list(self._purchases.values())

    def save_purchase(self, purchase: Purchase) -> Purchase:
        self._purchases[purchase.id] = purchase
        return purchase

    def clear(self) -> None:
        self._inventory.clear()
        self._recipes.clear()
        self._purchases.clear()


storage = InMemoryStorage()
