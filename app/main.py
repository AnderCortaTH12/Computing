from __future__ import annotations

from contextlib import asynccontextmanager
from typing import AsyncIterator

from fastapi import Depends, FastAPI

from app.core.security import get_current_username
from app.routers import inventory, notifications, purchases, recipes
from app.services.notifications import shutdown_scheduler, start_scheduler


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncIterator[None]:
    await start_scheduler()
    try:
        yield
    finally:
        await shutdown_scheduler()


def create_app() -> FastAPI:
    app = FastAPI(title="Inventory & Recipes API", lifespan=lifespan)

    app.include_router(inventory.router)
    app.include_router(recipes.router)
    app.include_router(purchases.router)
    app.include_router(notifications.router)

    @app.get("/")
    async def health(username: str = Depends(get_current_username)) -> dict:
        return {"message": f"Bienvenido, {username}!"}

    return app


app = create_app()
