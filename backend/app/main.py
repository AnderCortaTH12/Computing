from __future__ import annotations

from uuid import UUID

from fastapi import APIRouter, Depends, FastAPI, File, HTTPException, UploadFile
from fastapi.middleware.cors import CORSMiddleware
from sqlmodel import Session

from . import crud, database, models, ocr, parsers, schemas

app = FastAPI(title="Nevera Virtual API", docs_url="/api/docs", openapi_url="/api/openapi.json")

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=False,
"""FastAPI application exposing profile endpoints for the nutrition app."""
from __future__ import annotations

import os

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from .api import profile
from .database import init_db

app = FastAPI(title="Nutrition Profile API", version="0.1.0")

allowed_origins = [origin.strip() for origin in os.getenv("CORS_ALLOWED_ORIGINS", "*").split(",")]

app.add_middleware(
    CORSMiddleware,
    allow_origins=allowed_origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

api_router = APIRouter(prefix="/api")


@app.on_event("startup")
def on_startup() -> None:
    database.create_db_and_tables()
    with database.session_scope() as session:
        crud.seed_demo_content(session)


@api_router.get("/inventory", response_model=list[schemas.InventoryProductSummary])
def list_inventory(session: Session = Depends(database.get_session)):
    return crud.get_inventory_summary(session)


@api_router.post("/inventory/items", response_model=list[schemas.InventoryItemRead])
def create_inventory_items(
    payload: list[schemas.InventoryItemCreate], session: Session = Depends(database.get_session)
):
    created = crud.create_inventory_items(session, payload)
    product_cache: dict = {}
    response: list[schemas.InventoryItemRead] = []
    for item in created:
        product = product_cache.get(item.product_id)
        if product is None:
            product = session.get(models.Product, item.product_id)
            product_cache[item.product_id] = product
        product_name = product.name if product else ""
        unit = product.unit if product else "unidad"
        response.append(
            schemas.InventoryItemRead(
                id=item.id,
                product_id=item.product_id,
                product_name=product_name,
                quantity=item.quantity,
                unit=unit,
                expires_at=item.expires_at,
                notes=item.notes,
            )
        )
    return response


@api_router.patch("/inventory/items/{item_id}", response_model=schemas.InventoryItemRead)
def update_inventory_item(
    item_id: str, payload: schemas.InventoryItemUpdate, session: Session = Depends(database.get_session)
):
    try:
        item_uuid = UUID(item_id)
    except ValueError as exc:
        raise HTTPException(status_code=400, detail="Identificador de inventario inválido") from exc
    try:
        item = crud.update_inventory_item(session, item_id=item_uuid, payload=payload)
    except ValueError as exc:  # pragma: no cover - defensive
        raise HTTPException(status_code=404, detail=str(exc)) from exc

    product = session.get(models.Product, item.product_id)
    product_name = product.name if product else ""
    unit = product.unit if product else "unidad"
    return schemas.InventoryItemRead(
        id=item.id,
        product_id=item.product_id,
        product_name=product_name,
        quantity=item.quantity,
        unit=unit,
        expires_at=item.expires_at,
        notes=item.notes,
    )


@api_router.post("/inventory/receipt/parse-text", response_model=schemas.ReceiptParseResponse)
def parse_receipt_text(request: dict[str, str]):
    text = request.get("text", "")
    return parsers.parse_receipt_text(text)


@api_router.post("/inventory/receipt/scan", response_model=schemas.ReceiptParseResponse)
async def scan_receipt(file: UploadFile = File(...)):
    text, warnings = await ocr.extract_text_from_upload(file)
    parsed = parsers.parse_receipt_text(text)
    parsed.warnings.extend(warnings)
    return parsed


@api_router.post("/inventory/receipt/commit", response_model=list[schemas.InventoryItemRead])
def commit_receipt_items(
    payload: schemas.ReceiptCommitRequest, session: Session = Depends(database.get_session)
):
    entries = payload.items
    created = crud.create_inventory_items(session, entries)
    product_cache = {}
    result = []
    for item in created:
        product = product_cache.get(item.product_id)
        if product is None:
            product = session.get(models.Product, item.product_id)
            product_cache[item.product_id] = product
        product_name = product.name if product else ""
        unit = product.unit if product else "unidad"
        result.append(
            schemas.InventoryItemRead(
                id=item.id,
                product_id=item.product_id,
                product_name=product_name,
                quantity=item.quantity,
                unit=unit,
                expires_at=item.expires_at,
                notes=item.notes,
            )
        )
    return result


@api_router.get("/recipes", response_model=list[schemas.RecipeRead])
def list_recipes(session: Session = Depends(database.get_session)):
    return crud.get_recipe_availability(session)


app.include_router(api_router)
init_db()
app.include_router(profile.router)


@app.get("/health", tags=["health"]) 
def health_check() -> dict[str, str]:
    """Simple endpoint to confirm the service is running."""
    return {"status": "ok"}
