from __future__ import annotations

from collections import defaultdict
from datetime import date
from typing import Iterable, List, Sequence
from uuid import UUID

from sqlalchemy import func, select
from sqlmodel import Session
"""Data access helpers for the nutrition profile API."""
from __future__ import annotations

from sqlalchemy.orm import Session

from . import models, schemas


def normalize_name(name: str) -> str:
    return " ".join(name.strip().split())


def get_or_create_product(
    session: Session,
    *,
    name: str,
    unit: str,
    category: str | None = None,
    reorder_threshold: float | None = None,
) -> models.Product:
    normalized_name = normalize_name(name)
    product = session.exec(
        select(models.Product).where(func.lower(models.Product.name) == normalized_name.lower())
    ).first()
    if product:
        if product.unit != unit:
            product.unit = unit
        if category and product.category != category:
            product.category = category
        if reorder_threshold is not None and product.reorder_threshold != reorder_threshold:
            product.reorder_threshold = reorder_threshold
        session.add(product)
        session.commit()
        session.refresh(product)
        return product

    product = models.Product(
        name=normalized_name,
        unit=unit,
        category=category,
        reorder_threshold=reorder_threshold,
    )
    session.add(product)
    session.commit()
    session.refresh(product)
    return product


def create_inventory_items(
    session: Session, entries: Iterable[schemas.InventoryItemCreate]
) -> List[models.InventoryItem]:
    created: List[models.InventoryItem] = []
    for entry in entries:
        product = get_or_create_product(session, name=entry.product_name, unit=entry.unit)
        item = models.InventoryItem(
            product_id=product.id,
            quantity=entry.quantity,
            expires_at=entry.expires_at,
            notes=entry.notes,
        )
        session.add(item)
        session.commit()
        session.refresh(item)
        created.append(item)
    return created


def update_inventory_item(
    session: Session, item_id: UUID, payload: schemas.InventoryItemUpdate
) -> models.InventoryItem:
    item = session.get(models.InventoryItem, item_id)
    if not item:
        raise ValueError("Inventory item not found")
    if payload.quantity is not None:
        item.quantity = payload.quantity
    if payload.expires_at is not None:
        item.expires_at = payload.expires_at
    if payload.notes is not None:
        item.notes = payload.notes
    session.add(item)
    session.commit()
    session.refresh(item)
    return item


def delete_inventory_item(session: Session, item_id: UUID) -> None:
    item = session.get(models.InventoryItem, item_id)
    if item:
        session.delete(item)
        session.commit()


def get_inventory_summary(session: Session) -> List[schemas.InventoryProductSummary]:
    products = session.exec(select(models.Product)).all()
    products.sort(key=lambda product: product.name.lower())
    items = session.exec(select(models.InventoryItem)).all()

    items_by_product: dict[UUID, List[models.InventoryItem]] = defaultdict(list)
    for item in items:
        items_by_product[item.product_id].append(item)

    summaries: List[schemas.InventoryProductSummary] = []
    for product in products:
        product_items = items_by_product.get(product.id, [])
        product_items.sort(key=lambda item: (item.expires_at or date.max, item.quantity))
        total_quantity = sum(item.quantity for item in product_items)
        summaries.append(
            schemas.InventoryProductSummary(
                product_id=product.id,
                product_name=product.name,
                unit=product.unit,
                total_quantity=total_quantity,
                reorder_threshold=product.reorder_threshold,
                items=[
                    schemas.InventoryItemRead(
                        id=item.id,
                        product_id=item.product_id,
                        product_name=product.name,
                        quantity=item.quantity,
                        unit=product.unit,
                        expires_at=item.expires_at,
                        notes=item.notes,
                    )
                    for item in product_items
                ],
            )
        )
    return summaries


def seed_demo_content(session: Session) -> None:
    if session.exec(select(models.Product)).first():
        return

    milk = models.Product(name="Leche entera", unit="l", category="Lácteos")
    eggs = models.Product(name="Huevos", unit="unidad", category="Huevos", reorder_threshold=6)
    tomato = models.Product(name="Tomate", unit="kg", category="Verduras")
    pasta = models.Product(name="Pasta", unit="kg", category="Granos")
    cheese = models.Product(name="Queso parmesano", unit="kg", category="Lácteos")

    session.add_all([milk, eggs, tomato, pasta, cheese])
    session.commit()

    session.add_all(
        [
            models.InventoryItem(product_id=milk.id, quantity=3.0),
            models.InventoryItem(product_id=eggs.id, quantity=12),
            models.InventoryItem(product_id=tomato.id, quantity=1.5),
            models.InventoryItem(product_id=pasta.id, quantity=1.0),
            models.InventoryItem(product_id=cheese.id, quantity=0.3),
        ]
    )
    session.commit()

    pasta_recipe = models.Recipe(
        name="Pasta con tomate",
        description="Receta sencilla de pasta con salsa de tomate",
        servings=2,
        instructions=(
            "Cocer la pasta hasta que esté al dente. Sofreír tomate y mezclar con la pasta."
        ),
    )
    omelette_recipe = models.Recipe(
        name="Tortilla francesa",
        description="Clásica tortilla de huevo",
        servings=1,
        instructions="Batir huevos, cocinar en sartén antiadherente y doblar.",
    )

    session.add_all([pasta_recipe, omelette_recipe])
    session.commit()

    session.add_all(
        [
            models.RecipeIngredient(
                recipe_id=pasta_recipe.id,
                product_id=pasta.id,
                quantity_required=0.25,
            ),
            models.RecipeIngredient(
                recipe_id=pasta_recipe.id,
                product_id=tomato.id,
                quantity_required=0.4,
            ),
            models.RecipeIngredient(
                recipe_id=pasta_recipe.id,
                product_id=cheese.id,
                quantity_required=0.05,
            ),
            models.RecipeIngredient(
                recipe_id=omelette_recipe.id,
                product_id=eggs.id,
                quantity_required=2,
            ),
            models.RecipeIngredient(
                recipe_id=omelette_recipe.id,
                product_id=milk.id,
                quantity_required=0.1,
            ),
        ]
    )
    session.commit()


def get_recipe_availability(session: Session) -> List[schemas.RecipeRead]:
    products: Sequence[models.Product] = session.exec(select(models.Product)).all()
    product_map = {product.id: product for product in products}

    inventory_totals = {
        product_id: total
        for product_id, total in session.exec(
            select(models.InventoryItem.product_id, func.sum(models.InventoryItem.quantity)).group_by(
                models.InventoryItem.product_id
            )
        ).all()
    }

    recipes: Sequence[models.Recipe] = session.exec(select(models.Recipe)).all()
    ingredients_by_recipe: dict[UUID, List[models.RecipeIngredient]] = defaultdict(list)
    for ingredient in session.exec(select(models.RecipeIngredient)).all():
        ingredients_by_recipe[ingredient.recipe_id].append(ingredient)

    availability: List[schemas.RecipeRead] = []
    for recipe in recipes:
        missing: List[schemas.MissingIngredient] = []
        for ingredient in ingredients_by_recipe.get(recipe.id, []):
            product = product_map.get(ingredient.product_id)
            if not product:
                continue
            available_quantity = inventory_totals.get(ingredient.product_id, 0) or 0
            if available_quantity + 1e-6 < ingredient.quantity_required:
                missing.append(
                    schemas.MissingIngredient(
                        product_name=product.name,
                        required_quantity=ingredient.quantity_required,
                        available_quantity=available_quantity,
                        unit=product.unit,
                    )
                )
        availability.append(
            schemas.RecipeRead(
                id=recipe.id,
                name=recipe.name,
                description=recipe.description,
                servings=recipe.servings,
                instructions=recipe.instructions,
                can_make=len(missing) == 0,
                missing_ingredients=missing,
            )
        )
    return availability
def get_profile(db: Session, profile_id: int) -> models.UserProfile | None:
    return db.query(models.UserProfile).filter(models.UserProfile.id == profile_id).first()


def get_or_create_profile(db: Session, profile_id: int) -> models.UserProfile:
    profile = get_profile(db, profile_id)
    if profile is None:
        profile = models.UserProfile(id=profile_id)
        db.add(profile)
        db.commit()
        db.refresh(profile)
    return profile


def upsert_profile(db: Session, profile_id: int, payload: schemas.ProfileUpdate) -> models.UserProfile:
    profile = get_profile(db, profile_id)
    if profile is None:
        profile = models.UserProfile(id=profile_id)
        db.add(profile)

    profile.update_from_dict(payload.dict(exclude_unset=True))
    db.commit()
    db.refresh(profile)
    return profile
