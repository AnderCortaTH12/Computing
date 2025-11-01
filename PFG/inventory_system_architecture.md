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
| `notification_runs`  | `id` (UUID), `scheduled_for`, `executed_at`, `status` (`pending`/`success`/`failed`), `message_preview`, `llm_prompt_tokens`, `llm_completion_tokens`                    | Historial de ejecuciones del job serverless que genera mensajes diarios con OpenAI.                                    |
| `notification_events`| `id` (UUID), `user_id` (FK a `users`), `notification_run_id` (FK a `notification_runs`), `channel` (`chatgpt`/`webhook`/`expo`/`firebase`), `payload`, `sent_at`, `status` | Notificaciones emitidas a cada usuario (incluye cuerpo enviado y canal utilizado).                                     |
| `notification_interactions` | `id` (UUID), `notification_event_id` (FK a `notification_events`), `interaction_type` (`accept_meal`/`complete_training`/`dismiss`), `metadata`, `recorded_at`   | Registro de respuestas del usuario ante una notificación (aceptar comida, marcar entrenamiento, etc.).                  |

### Relaciones clave

- `products` se relaciona uno-a-muchos con `inventory_items` y `recipe_ingredients`.
- `recipes` se relaciona con `recipe_ingredients` (uno-a-muchos) y con `consumption_logs` (vía `reference_id` y `source='recipe'`).
- `shopping_lists` agrega necesidades usando `shopping_list_items`, y se enlaza con `purchase_orders`.
- `inventory_items` se alimenta de compras (al recibir un pedido se generan o actualizan registros) y se consume vía logs/recetas.
- `notification_runs` se relaciona uno-a-muchos con `notification_events`, mientras que `notification_events` se relaciona uno-a-muchos con `notification_interactions`.

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

### Notificaciones inteligentes
- `GET /notifications/daily`: consulta el último mensaje generado para el usuario autenticado y su estado de interacción.
- `POST /notifications/interactions`: registra acciones del usuario (aceptar comida, marcar entrenamiento completado, posponer).
- `POST /notifications/webhook`: endpoint opcional para recibir confirmaciones de entrega desde servicios externos de push.

## Automatización de mensajes diarios

Para generar mensajes personalizados se utilizará un job serverless programado (por ejemplo, AWS EventBridge Scheduler, Google Cloud Scheduler o Supabase Edge Functions con cron) que invoque una función (AWS Lambda/Cloud Function) cada mañana y tarde. El flujo propuesto es:

1. El scheduler envía el evento con metadatos de la ventana (mañana/noche) a la función serverless.
2. La función obtiene del backend (vía endpoint interno o acceso directo a la base) los datos agregados del inventario, progreso nutricional y objetivos del usuario.
3. Se construye un prompt estructurado para la API de OpenAI (modelo GPT-4.1, GPT-4o-mini u otro según presupuesto) que incluye reglas de tono, idioma y los datos dinámicos.
4. Se invoca `chat.completions.create` almacenando `prompt_tokens` y `completion_tokens` para monitorear costes en `notification_runs`.
5. El mensaje final se guarda y se publican eventos por usuario en `notification_events`, listos para ser enviados por los canales disponibles.

La infraestructura serverless permite escalar el número de usuarios y administrar ventanas horarias sin mantener servidores dedicados. Se configurarán reintentos automáticos y alarmas para `status="failed"` en CloudWatch/Error Reporting.

## Entrega multicanal de notificaciones

El backend expone un módulo de orquestación que determina el canal disponible por usuario:

- **App de ChatGPT / webhooks:** si la plataforma ofrece integración, se envía una petición POST autenticada con el cuerpo del mensaje. El resultado (HTTP 200/errores) se registra en `notification_events.status` y `payload`.
- **Push alternativo (Expo Push, Firebase Cloud Messaging):** se mantiene un token de dispositivo por usuario. El servicio serverless o una cola (p. ej. SNS + Lambda) publica la notificación. Se incluyen `data` JSON con identificadores para rastrear interacciones.
- **Fallback por email/SMS (opcional):** puede integrarse Twilio o un ESP si los push fallan repetidamente.

Para centralizar la observabilidad se usa una cola (ejemplo: SQS/Google Pub/Sub) donde el job serverless envía un mensaje por usuario; workers gestionados por el backend realizan la entrega y actualizan el estado, soportando reintentos idempotentes.

## Personalización con datos en tiempo real

El prompt enviado a OpenAI incluirá:

- Resumen del inventario crítico (productos con stock bajo o próximos a vencer).
- Progreso diario/semanal de ingesta calórica o macros.
- Objetivos activos (por ejemplo, completar 5 entrenamientos semanales) con porcentaje de avance.
- Historial reciente de interacciones del usuario (qué comidas aceptó o entrenamientos completó) para evitar repeticiones.

El backend expondrá servicios que agreguen estos datos en un formato compacto (JSON) y verificado, con límites para mantener el prompt por debajo de 8-10k tokens. Se implementarán validaciones para evitar inyectar información corrupta en el prompt (sanitización, truncamiento).

## Registro y seguimiento de interacciones

- Cada ejecución del job genera un registro en `notification_runs` con sello de tiempo programado y real, tokens usados y resumen del mensaje.
- La tabla `notification_events` almacena, por usuario, el estado del envío (`pending`, `delivered`, `failed`, `acknowledged`) y metadatos como ID de push o respuesta HTTP.
- Las respuestas de los usuarios llegan a través de endpoints (`POST /notifications/interactions`) o webhooks de Expo/Firebase, y se persisten en `notification_interactions`. Se normalizan los tipos de interacción (`accept_meal`, `complete_training`, `snooze`) y se vinculan al inventario/plan nutricional cuando corresponda.
- Se programan jobs analíticos (por ejemplo, cada noche) que procesan estas interacciones para ajustar recomendaciones futuras y alimentar dashboards.

Auditorías adicionales: se incluyen timestamps `created_at`/`updated_at` en todas las tablas, triggers para garantizar consistencia referencial y métricas en Prometheus/Grafana o BigQuery para medir engagement.

## Base de datos y migraciones

- **Motor:** PostgreSQL 15 en producción; SQLite para desarrollo y pruebas rápidas (manteniendo compatibilidad usando SQLAlchemy). PostgreSQL ofrece mejor concurrencia y extensiones útiles (por ejemplo, `pg_trgm` para búsqueda difusa de productos).
- **ORM / capa de persistencia:** SQLAlchemy 2.0 con modelo declarativo y Pydantic para esquemas de entrada/salida.
- **Migraciones:** Alembic. Se configurará con una rama principal y se crearán revisiones incrementales.
- **Migración inicial:**
  1. Crear tablas base (`users`, `products`, `inventory_items`, `recipes`, `recipe_ingredients`, `consumption_logs`, `shopping_lists`, `shopping_list_items`, `purchase_orders`, `inventory_adjustments`, `notification_runs`, `notification_events`, `notification_interactions`).
  2. Añadir índices para `products.name`, `inventory_items.expires_at`, `consumption_logs.logged_at`, `shopping_list_items.product_id`.
  3. Configurar constraints de clave externa con eliminación en cascada limitada (`ON DELETE SET NULL` para usuarios, `ON DELETE CASCADE` para dependencias estrictas).
- **Seeds iniciales:** script opcional para crear usuario administrador, categorías por defecto, unidades de medida comunes y preferencias/objetivos base que nutran la personalización de notificaciones.

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
