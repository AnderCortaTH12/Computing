from __future__ import annotations

from typing import List

from fastapi import APIRouter, Depends, status

from app.core.security import get_current_username
from app.models.purchase import Purchase, PurchaseCreate
from app.services import purchases as purchase_service

router = APIRouter(prefix="/purchases", tags=["purchases"], dependencies=[Depends(get_current_username)])


@router.get("/", response_model=List[Purchase])
def list_purchases() -> List[Purchase]:
    return purchase_service.list_purchases()


@router.post("/", response_model=Purchase, status_code=status.HTTP_201_CREATED)
def create_purchase(payload: PurchaseCreate) -> Purchase:
    return purchase_service.create_purchase(payload)
