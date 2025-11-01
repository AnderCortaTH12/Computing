from __future__ import annotations

import os
from contextlib import contextmanager

from sqlmodel import Session, SQLModel, create_engine


DATABASE_URL = os.getenv("INVENTORY_DATABASE_URL", "sqlite:///./inventory.db")

engine = create_engine(
    DATABASE_URL,
    echo=False,
    connect_args={"check_same_thread": False} if DATABASE_URL.startswith("sqlite") else {},
)


def create_db_and_tables() -> None:
    """Ensure that the database schema exists."""

    SQLModel.metadata.create_all(engine)


@contextmanager
def session_scope():
    session = Session(engine)
    try:
        yield session
    finally:
        session.close()


def get_session():
    with Session(engine) as session:
        yield session
