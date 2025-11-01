"""SQLAlchemy models for the nutrition profile domain."""
from __future__ import annotations

from datetime import datetime

from sqlalchemy import Column, DateTime, Float, Integer, Text

from .database import Base


class UserProfile(Base):
    """Store anthropometric data and preferences for a user."""

    __tablename__ = "user_profiles"

    id = Column(Integer, primary_key=True, index=True)
    weight = Column(Float, nullable=True)
    height = Column(Float, nullable=True)
    goals = Column(Text, nullable=True)
    dietary_restrictions = Column(Text, nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow, nullable=False)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow, nullable=False)

    def update_from_dict(self, data: dict[str, object]) -> None:
        """Apply updates using a dict produced by a Pydantic model."""
        for key, value in data.items():
            setattr(self, key, value)
