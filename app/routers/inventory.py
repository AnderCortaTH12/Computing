from __future__ import annotations

from typing import List
from uuid import UUID

from fastapi import APIRouter, Depends, status

from app.core.security import get_current_username
from app.models.inventory import InventoryCreate, InventoryItem, InventoryUpdate
from app.services import inventory as inventory_service

router = APIRouter(prefix="/inventory", tags=["inventory"], dependencies=[Depends(get_current_username)])


@router.get("/", response_model=List[InventoryItem])
def list_inventory() -> List[InventoryItem]:
    return inventory_service.list_inventory()


@router.post("/", response_model=InventoryItem, status_code=status.HTTP_201_CREATED)
def create_inventory_item(payload: InventoryCreate) -> InventoryItem:
    return inventory_service.create_inventory_item(payload)


@router.get("/{item_id}", response_model=InventoryItem)
def get_inventory_item(item_id: UUID) -> InventoryItem:
    return inventory_service.get_inventory_item(item_id)


@router.put("/{item_id}", response_model=InventoryItem)
def update_inventory_item(item_id: UUID, payload: InventoryUpdate) -> InventoryItem:
    return inventory_service.update_inventory_item(item_id, payload)


@router.delete("/{item_id}", status_code=status.HTTP_204_NO_CONTENT)
def delete_inventory_item(item_id: UUID) -> None:
    inventory_service.delete_inventory_item(item_id)
