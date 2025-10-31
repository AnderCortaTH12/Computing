from __future__ import annotations

from typing import Dict, List

from app.models.recipe import Recipe, RecipeIngredient
from app.services.storage import storage


class RecipeRecommendation:
    def __init__(self, recipe: Recipe, missing: Dict[str, float]) -> None:
        self.recipe = recipe
        self.missing = missing

    @property
    def can_prepare(self) -> bool:
        return not self.missing

    def to_dict(self) -> Dict[str, object]:
        return {
            "recipe": self.recipe,
            "can_prepare": self.can_prepare,
            "missing": self.missing,
        }


def _inventory_index() -> Dict[str, float]:
    index: Dict[str, float] = {}
    for item in storage.list_inventory():
        key = f"{item.name.lower()}::{item.unit.lower()}"
        index[key] = item.quantity
    return index


def _ingredient_key(ingredient: RecipeIngredient) -> str:
    return f"{ingredient.name.lower()}::{ingredient.unit.lower()}"


def recommend_recipes() -> List[Dict[str, object]]:
    inventory_quantities = _inventory_index()
    recommendations: List[RecipeRecommendation] = []

    for recipe in storage.list_recipes():
        missing: Dict[str, float] = {}
        for ingredient in recipe.ingredients:
            key = _ingredient_key(ingredient)
            available = inventory_quantities.get(key, 0)
            if available < ingredient.quantity:
                missing_amount = round(ingredient.quantity - available, 2)
                missing[ingredient.name] = missing_amount
        recommendations.append(RecipeRecommendation(recipe, missing))

    return [recommendation.to_dict() for recommendation in recommendations]
