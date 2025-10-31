from __future__ import annotations

from typing import List
from uuid import UUID

from fastapi import HTTPException, status

from app.models.recipe import Recipe, RecipeCreate, RecipeUpdate
from app.services.storage import storage


def list_recipes() -> List[Recipe]:
    return storage.list_recipes()


def create_recipe(payload: RecipeCreate) -> Recipe:
    recipe = Recipe(**payload.dict())
    storage.save_recipe(recipe)
    return recipe


def get_recipe(recipe_id: UUID) -> Recipe:
    recipe = storage.get_recipe(recipe_id)
    if not recipe:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Recipe not found")
    return recipe


def update_recipe(recipe_id: UUID, payload: RecipeUpdate) -> Recipe:
    recipe = get_recipe(recipe_id)
    updated_data = recipe.dict()
    for key, value in payload.dict(exclude_unset=True).items():
        updated_data[key] = value
    updated_recipe = Recipe(**updated_data)
    storage.save_recipe(updated_recipe)
    return updated_recipe


def delete_recipe(recipe_id: UUID) -> None:
    if not storage.get_recipe(recipe_id):
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Recipe not found")
    storage.delete_recipe(recipe_id)
