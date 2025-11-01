"""Pydantic schemas for request and response bodies."""
from __future__ import annotations

from datetime import datetime
from typing import Optional

from pydantic import BaseModel, Field, validator


class ProfileBase(BaseModel):
    weight: Optional[float] = Field(None, description="Peso corporal en kilogramos")
    height: Optional[float] = Field(None, description="Altura en centímetros")
    goals: Optional[str] = Field(None, description="Objetivos personales o deportivos")
    dietary_restrictions: Optional[str] = Field(None, description="Restricciones o preferencias alimentarias")

    @validator("weight", "height")
    def _validate_positive(cls, value: Optional[float]) -> Optional[float]:
        if value is not None and value <= 0:
            raise ValueError("El valor debe ser mayor que cero")
        return value


class ProfileUpdate(ProfileBase):
    """Schema used to update or create a profile."""


class Profile(ProfileBase):
    id: int
    created_at: datetime
    updated_at: datetime

    class Config:
        orm_mode = True
