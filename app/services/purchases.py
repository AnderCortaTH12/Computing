from __future__ import annotations

from typing import List

from app.models.inventory import InventoryUpdate
from app.models.purchase import Purchase, PurchaseCreate
from app.services import inventory
from app.services.storage import storage


def list_purchases() -> List[Purchase]:
    return storage.list_purchases()


def create_purchase(payload: PurchaseCreate) -> Purchase:
    item = inventory.get_inventory_item(payload.inventory_item_id)
    updated_quantity = item.quantity + payload.quantity
    inventory.update_inventory_item(
        payload.inventory_item_id,
        InventoryUpdate(quantity=updated_quantity),
    )
    purchase = Purchase(**payload.dict())
    storage.save_purchase(purchase)
    return purchase
