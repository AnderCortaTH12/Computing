from __future__ import annotations

from typing import List
from uuid import UUID

from fastapi import APIRouter, Depends, status

from app.core.security import get_current_username
from app.models.recipe import Recipe, RecipeCreate, RecipeUpdate
from app.services import recipes as recipe_service
from app.services import recommendations as recommendation_service

router = APIRouter(prefix="/recipes", tags=["recipes"], dependencies=[Depends(get_current_username)])


@router.get("/", response_model=List[Recipe])
def list_recipes() -> List[Recipe]:
    return recipe_service.list_recipes()


@router.post("/", response_model=Recipe, status_code=status.HTTP_201_CREATED)
def create_recipe(payload: RecipeCreate) -> Recipe:
    return recipe_service.create_recipe(payload)


@router.get("/{recipe_id}", response_model=Recipe)
def get_recipe(recipe_id: UUID) -> Recipe:
    return recipe_service.get_recipe(recipe_id)


@router.put("/{recipe_id}", response_model=Recipe)
def update_recipe(recipe_id: UUID, payload: RecipeUpdate) -> Recipe:
    return recipe_service.update_recipe(recipe_id, payload)


@router.delete("/{recipe_id}", status_code=status.HTTP_204_NO_CONTENT)
def delete_recipe(recipe_id: UUID) -> None:
    recipe_service.delete_recipe(recipe_id)


@router.get("/recommendations", response_model=List[dict])
def recipe_recommendations() -> List[dict]:
    return recommendation_service.recommend_recipes()
