# Sistema de gestión de inventario culinario

## Stack seleccionado

- **Backend:** [FastAPI](https://fastapi.tiangolo.com/) con Python 3.11. Se ejecutará con Uvicorn como servidor ASGI y utilizará Pydantic v2 para validación de datos. Se gestionará el entorno con Poetry para mantener dependencias y scripts.
- **Frontend:** React 18 con TypeScript, empaquetado mediante Vite. Se aplicará Chakra UI para componentes reutilizables y React Query para el manejo de estado de datos remotos.
- **Autenticación y autorización:** JSON Web Tokens (JWT) firmados con claves RSA. FastAPI proporcionará dependencias para la verificación de tokens y roles (usuario estándar, administrador).
- **Infraestructura complementaria:** Docker Compose para orquestar servicios (API, frontend, base de datos), y GitHub Actions para CI (lint + pruebas unitarias + verificación de migraciones).

## Modelo de datos

| Entidad              | Campos principales                                                                                                                                                         | Descripción                                                                                                            |
|----------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------|
| `users`              | `id` (UUID), `email` (único), `hashed_password`, `role` (`admin`/`user`), `created_at`, `updated_at`                                                                       | Personas con acceso al sistema. `role` define permisos (solo administradores pueden crear recetas).                    |
| `products`           | `id` (UUID), `name`, `category`, `unit` (kg, unidad, l), `reorder_threshold`, `calories_per_unit`, `created_at`, `updated_at`                                              | Catálogo maestro de productos disponibles para recetas y compras.                                                      |
| `inventory_items`    | `id` (UUID), `product_id` (FK a `products`), `quantity`, `batch_code`, `expires_at`, `storage_location`, `created_at`, `updated_at`                                        | Existencias actuales, diferenciadas por lote/fecha de caducidad.                                                       |
| `recipes`            | `id` (UUID), `name`, `instructions` (Markdown), `tags` (array), `servings`, `created_by` (FK a `users`), `created_at`, `updated_at`                                       | Recetas guardadas en el sistema.                                                                                       |
| `recipe_ingredients` | `id` (UUID), `recipe_id` (FK a `recipes`), `product_id` (FK a `products`), `quantity_required`                                                                            | Tabla puente que indica los productos necesarios para cada receta.                                                     |
| `consumption_logs`   | `id` (UUID), `inventory_item_id` (FK a `inventory_items`), `product_id` (FK a `products`), `quantity_used`, `source` (`recipe`/`manual`), `reference_id`, `logged_at`      | Historial de consumo del inventario, vinculando recetas ejecutadas o consumos manuales.                                |
| `shopping_lists`     | `id` (UUID), `name`, `status` (`draft`/`ordered`/`received`), `created_by` (FK a `users`), `created_at`, `updated_at`                                                      | Listas de compras activas.                                                                                             |
| `shopping_list_items`| `id` (UUID), `shopping_list_id` (FK a `shopping_lists`), `product_id` (FK a `products`), `quantity_needed`, `quantity_ordered`, `quantity_received`                        | Productos requeridos para abastecer inventario según recetas y umbrales.                                               |
| `purchase_orders`    | `id` (UUID), `shopping_list_id` (FK a `shopping_lists`), `supplier_name`, `ordered_at`, `received_at`, `invoice_number`, `notes`                                          | Registro de compras asociadas a una lista.                                                                             |
| `inventory_adjustments` | `id` (UUID), `inventory_item_id` (FK a `inventory_items`), `change` (positivo/negativo), `reason`, `adjusted_by` (FK a `users`), `adjusted_at`                        | Movimientos manuales para ajustar el inventario (mermas, correcciones).                                                |

### Relaciones clave

- `products` se relaciona uno-a-muchos con `inventory_items` y `recipe_ingredients`.
- `recipes` se relaciona con `recipe_ingredients` (uno-a-muchos) y con `consumption_logs` (vía `reference_id` y `source='recipe'`).
- `shopping_lists` agrega necesidades usando `shopping_list_items`, y se enlaza con `purchase_orders`.
- `inventory_items` se alimenta de compras (al recibir un pedido se generan o actualizan registros) y se consume vía logs/recetas.

## Endpoints REST propuestos

### Autenticación y usuarios
- `POST /auth/login`: devuelve tokens de acceso/refresh.
- `POST /auth/refresh`: renueva el token de acceso.
- `POST /users`: (admin) crea usuarios.
- `GET /users/me`: datos del usuario autenticado.

### Productos e inventario
- `GET /products`: listado filtrable/paginado.
- `POST /products`: (admin) crea un producto.
- `GET /products/{product_id}`: detalle.
- `PATCH /products/{product_id}`: actualiza campos.
- `DELETE /products/{product_id}`: (admin) desactiva o elimina.
- `GET /inventory`: estado actual del inventario agrupado por producto o lote.
- `POST /inventory`: agrega lote manual (recepciones fuera de flujo de compra).
- `PATCH /inventory/{inventory_item_id}`: ajustes de cantidad, ubicación o caducidad.

### Recetas y sugerencias
- `GET /recipes`: listado con filtros por tags, ingredientes disponibles.
- `POST /recipes`: (admin) crea receta.
- `GET /recipes/{recipe_id}`: detalle completo con ingredientes.
- `PATCH /recipes/{recipe_id}`: actualizar.
- `DELETE /recipes/{recipe_id}`: archivar/eliminar.
- `GET /recipes/suggestions`: devuelve recetas factibles según inventario actual y preferencias (parámetros: `servings`, `max_missing_items`).

### Consumo e historial
- `POST /consumption`: registra consumo manual o ejecución de receta (requiere `source` y `reference_id`).
- `GET /consumption`: historial con filtros por rango de fechas, producto o receta.
- `POST /consumption/{log_id}/reverse`: revierte un movimiento erróneo (crea ajuste).

### Listas de la compra y pedidos
- `GET /shopping-lists`: listas activas.
- `POST /shopping-lists`: crea nueva lista (permite generar desde recetas o umbrales).
- `GET /shopping-lists/{list_id}`: detalle con items.
- `PATCH /shopping-lists/{list_id}`: actualizar estado o metadatos.
- `POST /shopping-lists/{list_id}/items`: agregar producto necesario.
- `PATCH /shopping-list-items/{item_id}`: actualizar cantidades solicitadas/recibidas.
- `DELETE /shopping-list-items/{item_id}`: eliminar ítem.
- `POST /shopping-lists/{list_id}/generate-order`: genera orden de compra.
- `GET /purchase-orders`: listado de pedidos.
- `GET /purchase-orders/{order_id}`: detalle y trazabilidad.
- `POST /purchase-orders/{order_id}/receive`: registra recepción parcial/total y crea/actualiza `inventory_items`.

### Integraciones y métricas
- `GET /reports/stock-levels`: inventario resumido con alertas de umbral.
- `GET /reports/expiring`: lotes próximos a caducar.
- `GET /reports/consumption-trends`: consumo agregado por periodo.
- `POST /webhooks/supplier`: endpoint para actualizaciones externas (por ejemplo, confirmación de pedidos).

## Base de datos y migraciones

- **Motor:** PostgreSQL 15 en producción; SQLite para desarrollo y pruebas rápidas (manteniendo compatibilidad usando SQLAlchemy). PostgreSQL ofrece mejor concurrencia y extensiones útiles (por ejemplo, `pg_trgm` para búsqueda difusa de productos).
- **ORM / capa de persistencia:** SQLAlchemy 2.0 con modelo declarativo y Pydantic para esquemas de entrada/salida.
- **Migraciones:** Alembic. Se configurará con una rama principal y se crearán revisiones incrementales.
- **Migración inicial:**
  1. Crear tablas base (`users`, `products`, `inventory_items`, `recipes`, `recipe_ingredients`, `consumption_logs`, `shopping_lists`, `shopping_list_items`, `purchase_orders`, `inventory_adjustments`).
  2. Añadir índices para `products.name`, `inventory_items.expires_at`, `consumption_logs.logged_at`, `shopping_list_items.product_id`.
  3. Configurar constraints de clave externa con eliminación en cascada limitada (`ON DELETE SET NULL` para usuarios, `ON DELETE CASCADE` para dependencias estrictas).
- **Seeds iniciales:** script opcional para crear usuario administrador, categorías por defecto y unidades de medida comunes.

## Estructura propuesta del repositorio

```text
inventory-app/
├── backend/
│   ├── pyproject.toml
│   ├── alembic/
│   ├── app/
│   │   ├── main.py
│   │   ├── api/
│   │   ├── core/
│   │   ├── models/
│   │   ├── schemas/
│   │   └── services/
│   └── tests/
├── frontend/
│   ├── package.json
│   ├── src/
│   │   ├── App.tsx
│   │   ├── components/
│   │   └── features/
└── docker-compose.yml
```

Esta estructura separa las responsabilidades y permite desplegar backend y frontend de forma independiente o conjunta.
