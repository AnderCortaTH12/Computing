"""API routes for user profile operations."""
from __future__ import annotations

from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.orm import Session

from .. import crud, schemas
from ..database import get_session

router = APIRouter(prefix="/profile", tags=["profile"])


@router.get("/{profile_id}", response_model=schemas.Profile)
def read_profile(profile_id: int, db: Session = Depends(get_session)):
    """Return the profile data for the given identifier."""
    profile = crud.get_or_create_profile(db, profile_id)
    return profile


@router.put("/{profile_id}", response_model=schemas.Profile, status_code=status.HTTP_200_OK)
def update_profile(
    profile_id: int,
    payload: schemas.ProfileUpdate,
    db: Session = Depends(get_session),
):
    """Create or update the profile with the provided information."""
    if payload is None:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Datos no proporcionados")

    profile = crud.upsert_profile(db, profile_id, payload)
    return profile
