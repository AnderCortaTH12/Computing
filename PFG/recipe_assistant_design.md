# Módulo de asistente culinario con IA

Este documento describe cómo integrar un asistente basado en OpenAI para sugerir recetas según ingredientes disponibles, objetivos nutricionales y preferencias del usuario. También detalla el flujo para descontar inventario, registrar el historial alimenticio y la propuesta de interfaz de usuario.

## 1. Plantillas de prompt para OpenAI

> Objetivo: obtener hasta 5 recetas que utilicen los ingredientes disponibles, respeten las restricciones nutricionales y se ajusten al objetivo del usuario (p. ej. pérdida de peso, ganancia muscular, comida vegetariana).

### 1.1 Prompt del sistema

```text
Eres un asistente culinario profesional. Diseñas recetas viables con ingredientes reales, indicas cantidades exactas para 1 ración y calculas macros (calorías, proteínas, carbohidratos, grasas). Tus respuestas deben estar en JSON válido con el siguiente esquema:
{
  "recipes": [
    {
      "name": str,
      "summary": str,
      "ingredients": [
        {"name": str, "quantity": float, "unit": str},
      ],
      "instructions": [str],
      "macros": {"calories": int, "protein": int, "carbs": int, "fat": int},
      "tags": [str]
    }
  ]
}
Si no puedes generar una receta válida, responde con {"recipes": []} y explica el motivo en el campo "summary".
```

### 1.2 Prompt del usuario (plantilla)

```jinja2
Genera hasta {{ max_recipes }} recetas que usen exclusivamente los ingredientes disponibles y se ajusten al objetivo nutricional.

Ingredientes disponibles:
{% for item in pantry %}
- {{ item.name }}: {{ item.quantity }} {{ item.unit }} (caduca: {{ item.expires_at }})
{% endfor %}

Objetivo del usuario: {{ goal }}
Restricciones alimenticias: {{ dietary_restrictions | default("Ninguna") }}
Tiempo máximo de preparación: {{ max_prep_minutes }} minutos
Número de comensales: {{ servings }}

Evita repetir ingredientes agotados o combinar restricciones incompatibles.
```

### 1.3 Reintentos y validación

1. Enviar prompt al endpoint `responses` de OpenAI con temperatura 0.7 y límite de 1500 tokens.
2. Validar JSON contra un esquema Pydantic/TypeScript.
3. Si la validación falla, solicitar corrección automática con el mismo mensaje del sistema añadiendo:
   - "Tu respuesta anterior no era JSON válido. Genera la misma información en JSON válido según el esquema indicado".

## 2. Flujo para seleccionar recetas y descontar inventario

### 2.1 API Backend

| Método | Ruta | Descripción |
|--------|------|-------------|
| `POST` | `/ai/recipes/suggest` | Llama al cliente OpenAI, guarda la respuesta en la tabla `recipe_suggestions` y devuelve la lista.
| `POST` | `/ai/recipes/select` | Recibe `suggestion_id`, `recipe_index` y `servings`. Valida inventario, descuenta cantidades y crea registro en `meal_logs`.
| `POST` | `/inventory/transactions` | (Existente) Permite ajustes manuales cuando faltan ingredientes.

### 2.2 Pseudocódigo de selección

```python
@router.post("/ai/recipes/select")
def select_recipe(payload: SelectRecipeRequest, db: Session = Depends(get_db)):
    suggestion = repo.get_suggestion(db, payload.suggestion_id)
    recipe = suggestion.recipes[payload.recipe_index]

    required_items = scale_ingredients(recipe.ingredients, payload.servings)
    inventory_updates = inventory_service.reserve_items(db, required_items)

    meal_log = meal_logs_repo.create(
        db,
        user_id=current_user.id,
        recipe_name=recipe.name,
        servings=payload.servings,
        macros=scale_macros(recipe.macros, payload.servings),
        consumed_at=payload.consumed_at or datetime.utcnow()
    )

    db.commit()
    return {"meal_log": meal_log, "inventory_updates": inventory_updates}
```

### 2.3 Descuento de inventario

1. Convertir cada ingrediente de la receta a `product_id` mediante coincidencia exacta (`products.name`) o alias.
2. Verificar lotes disponibles ordenados por caducidad (`FIFO` por fecha).
3. Restar cantidades desde `inventory_items` creando registros en `consumption_logs` con `source='recipe'` y `reference_id=meal_log.id`.
4. Si la cantidad no alcanza, devolver error `409 Conflict` con detalle del faltante para permitir sustituciones.

## 3. Registro del historial alimenticio

### 3.1 Esquema de base de datos

| Tabla | Campos | Notas |
|-------|--------|-------|
| `meal_logs` | `id` (UUID), `user_id`, `recipe_name`, `servings`, `macros_calories`, `macros_protein`, `macros_carbs`, `macros_fat`, `consumed_at`, `created_at` | Guarda la elección del usuario y macros agregadas.
| `recipe_suggestions` | `id`, `user_id`, `raw_response` (JSONB), `created_at`, `goal`, `filters` | Persistencia de la respuesta para auditoría y re-selección.
| `meal_log_items` | `id`, `meal_log_id`, `product_id`, `quantity_used`, `unit` | Detalle por ingrediente para reportes de inventario/nutrición.

### 3.2 Actualización de macros

- Al confirmar la receta, sumar los macros escalados a los totales diarios del usuario (`daily_nutrition_totals`).
- Permitir revertir una selección creando un endpoint `DELETE /meal-logs/{id}` que revierta inventario y totales.

## 4. Diseño de la interfaz de usuario

### 4.1 Layout general

- **Sección izquierda:** filtros (objetivo nutricional, tiempo, restricciones, calorías máximas, ingredientes opcionales/evitar).
- **Sección central:** lista de recetas sugeridas con tarjetas plegables.
- **Sección derecha:** inventario relevante y resumen diario de macros.

### 4.2 Componentes clave (React + Chakra UI)

| Componente | Props principales | Función |
|------------|-------------------|---------|
| `RecipeFiltersPanel` | `initialFilters`, `onSubmit` | Formulario con selectores y chips para restricciones; dispara `suggestRecipes`.
| `RecipeSuggestionsList` | `suggestions`, `onSelect` | Renderiza tarjetas. Cada tarjeta muestra resumen, tags y botón "Ver detalle".
| `RecipeDetailDrawer` | `recipe`, `isOpen`, `onClose`, `onConfirm(servings, consumedAt)` | Paso de confirmación con selector de porciones y macros calculadas en vivo.
| `InventoryImpactTable` | `ingredients`, `availableStock` | Indica qué lotes se consumirán y si faltan unidades.
| `DailyNutritionSummary` | `totals`, `targets` | Barra de progreso de calorías y macros.

### 4.3 Flujo de interacción

1. Usuario ajusta filtros y pulsa "Generar recetas" → llamada a `/ai/recipes/suggest`.
2. Se muestran tarjetas con nombre, resumen, macros por ración y chips de tags.
3. Al pulsar "Elegir", se abre el `RecipeDetailDrawer` mostrando ingredientes escalables según porciones.
4. Botón "Confirmar" llama a `/ai/recipes/select`.
   - Mostrar `InventoryImpactTable` dentro del drawer antes de confirmar.
   - Tras éxito, mostrar `Toast` con confirmación y actualizar lista de inventario + resumen de macros.
5. Registrar en historial visible en pestaña "Historial" con tabla (`MealHistoryTable`) filtrable por fecha y metas.

### 4.4 Estados y manejo de errores

- Mostrar mensaje amigable cuando el modelo no devuelva recetas (`recipes.length === 0`).
- Si el backend retorna `409` por faltantes, resaltar ingredientes y sugerir reducir porciones o agregar a lista de compras.
- Confirmación modal adicional cuando el consumo supere el objetivo calórico diario >10%.

### 4.5 Accesibilidad y UX

- Navegación por teclado en tarjetas y drawer.
- Contraste AA para textos y botones.
- Lectores de pantalla: usar `aria-live` para notificar confirmaciones y errores.

## 5. Consideraciones adicionales

- Cachear respuestas de OpenAI por usuario y filtros durante 15 minutos para evitar costos excesivos.
- Registrar en `audit_logs` la acción "AI_RECIPE_SELECTED" con `user_id`, `recipe_name`, `servings` y `tokens_used`.
- Añadir pruebas unitarias del servicio de inventario para validar descuentos en múltiples lotes y condiciones de stock insuficiente.

