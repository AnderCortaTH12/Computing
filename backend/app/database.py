"""Database configuration utilities for the nutrition profile service."""
from __future__ import annotations

import os
from pathlib import Path
from typing import Generator

from sqlalchemy import create_engine
from sqlalchemy.orm import declarative_base, sessionmaker

DEFAULT_SQLITE_PATH = Path(__file__).resolve().parent / "data" / "app.db"

def _build_database_url() -> str:
    """Return the database URL, defaulting to a local SQLite file."""
    url = os.getenv("PROFILE_DATABASE_URL")
    if url:
        return url

    data_dir = DEFAULT_SQLITE_PATH.parent
    data_dir.mkdir(parents=True, exist_ok=True)
    return f"sqlite:///{DEFAULT_SQLITE_PATH}"

DATABASE_URL = _build_database_url()

connect_args = {"check_same_thread": False} if DATABASE_URL.startswith("sqlite") else {}

engine = create_engine(DATABASE_URL, connect_args=connect_args, future=True)
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine, future=True)

Base = declarative_base()


def get_session() -> Generator:
    """Yield a database session that closes automatically."""
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


def init_db() -> None:
    """Create all tables if they do not exist yet."""
    from . import models  # noqa: F401  Ensure models are registered

    Base.metadata.create_all(bind=engine)
