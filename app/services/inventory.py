from __future__ import annotations

from typing import List
from uuid import UUID

from fastapi import HTTPException, status

from app.models.inventory import InventoryCreate, InventoryItem, InventoryUpdate
from app.services.storage import storage


def list_inventory() -> List[InventoryItem]:
    return storage.list_inventory()


def create_inventory_item(payload: InventoryCreate) -> InventoryItem:
    item = InventoryItem(**payload.dict())
    storage.save_inventory(item)
    return item


def get_inventory_item(item_id: UUID) -> InventoryItem:
    item = storage.get_inventory(item_id)
    if not item:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Inventory item not found")
    return item


def update_inventory_item(item_id: UUID, payload: InventoryUpdate) -> InventoryItem:
    item = get_inventory_item(item_id)
    updated_data = item.dict()
    for key, value in payload.dict(exclude_unset=True).items():
        updated_data[key] = value
    updated_item = InventoryItem(**updated_data)
    storage.save_inventory(updated_item)
    return updated_item


def delete_inventory_item(item_id: UUID) -> None:
    if not storage.get_inventory(item_id):
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Inventory item not found")
    storage.delete_inventory(item_id)
