"""Data access helpers for the nutrition profile API."""
from __future__ import annotations

from sqlalchemy.orm import Session

from . import models, schemas


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
