from __future__ import annotations

from typing import List

from fastapi import APIRouter, Depends

from app.core.security import get_current_username
from app.services.notifications import get_notifications

router = APIRouter(prefix="/notifications", tags=["notifications"], dependencies=[Depends(get_current_username)])


@router.get("/", response_model=List[str])
def list_notifications() -> List[str]:
    return get_notifications()
