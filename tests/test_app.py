from __future__ import annotations

import base64

import pytest
from fastapi.testclient import TestClient

from app.main import app
from app.services.notifications import clear_notifications
from app.services.storage import storage

client = TestClient(app)


def _auth_headers() -> dict[str, str]:
    token = base64.b64encode(b"admin:secret").decode()
    return {"Authorization": f"Basic {token}"}


@pytest.fixture(autouse=True)
def clear_state() -> None:
    storage.clear()
    clear_notifications()


def test_inventory_crud_flow() -> None:
    response = client.post(
        "/inventory/",
        json={"name": "Harina", "quantity": 2.0, "unit": "kg"},
        headers=_auth_headers(),
    )
    assert response.status_code == 201, response.text
    item = response.json()
    item_id = item["id"]

    response = client.get("/inventory/", headers=_auth_headers())
    assert response.status_code == 200
    assert len(response.json()) == 1

    response = client.put(
        f"/inventory/{item_id}",
        json={"quantity": 1.5},
        headers=_auth_headers(),
    )
    assert response.status_code == 200
    assert response.json()["quantity"] == 1.5

    response = client.delete(f"/inventory/{item_id}", headers=_auth_headers())
    assert response.status_code == 204


def test_recipe_and_recommendations_flow() -> None:
    inv_resp = client.post(
        "/inventory/",
        json={"name": "Huevos", "quantity": 4, "unit": "unidad"},
        headers=_auth_headers(),
    )
    assert inv_resp.status_code == 201

    recipe_resp = client.post(
        "/recipes/",
        json={
            "name": "Tortilla",
            "description": "Receta de tortilla",
            "ingredients": [
                {"name": "Huevos", "quantity": 3, "unit": "unidad"},
                {"name": "Patatas", "quantity": 2, "unit": "unidad"},
            ],
        },
        headers=_auth_headers(),
    )
    assert recipe_resp.status_code == 201

    rec_resp = client.get("/recipes/recommendations", headers=_auth_headers())
    assert rec_resp.status_code == 200
    body = rec_resp.json()
    assert isinstance(body, list)
    assert body[0]["recipe"]["name"] == "Tortilla"
    assert body[0]["can_prepare"] is False
    assert body[0]["missing"]["Patatas"] == 2


def test_purchase_updates_inventory() -> None:
    inv_resp = client.post(
        "/inventory/",
        json={"name": "Leche", "quantity": 1, "unit": "L"},
        headers=_auth_headers(),
    )
    item_id = inv_resp.json()["id"]

    purchase_resp = client.post(
        "/purchases/",
        json={"inventory_item_id": item_id, "quantity": 2},
        headers=_auth_headers(),
    )
    assert purchase_resp.status_code == 201

    updated_item = client.get(f"/inventory/{item_id}", headers=_auth_headers()).json()
    assert updated_item["quantity"] == 3


def test_notifications_endpoint_returns_list() -> None:
    response = client.get("/notifications/", headers=_auth_headers())
    assert response.status_code == 200
    assert isinstance(response.json(), list)
