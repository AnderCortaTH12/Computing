const API_BASE = window.APP_CONFIG?.API_BASE ?? "http://localhost:8000/api";

const inventoryBody = document.getElementById("inventory-body");
const inventorySearch = document.getElementById("inventory-search");
const manualEntryForm = document.getElementById("manual-entry-form");
const receiptTextForm = document.getElementById("receipt-text-form");
const receiptScanForm = document.getElementById("receipt-scan-form");
const receiptResults = document.getElementById("receipt-results");
const receiptWarnings = document.getElementById("receipt-warnings");
const receiptItemsBody = document.getElementById("receipt-items-body");
const receiptItemsForm = document.getElementById("receipt-items-form");
const recipesList = document.getElementById("recipes");
const apiStatus = document.getElementById("api-status");

let currentInventory = [];
let receiptItems = [];

function formatQuantity(value, unit) {
  const formatter = new Intl.NumberFormat("es-ES", { maximumFractionDigits: 2 });
  return `${formatter.format(value)} ${unit}`;
}

function updateApiStatus(connected) {
  if (connected) {
    apiStatus.textContent = "Backend disponible";
    apiStatus.classList.add("connected");
  } else {
    apiStatus.textContent = "Sin conexión con el backend";
    apiStatus.classList.remove("connected");
  }
}

async function fetchInventory() {
  try {
    const response = await fetch(`${API_BASE}/inventory`);
    if (!response.ok) throw new Error("Error al obtener inventario");
    const data = await response.json();
    currentInventory = data;
    renderInventory(data);
    updateApiStatus(true);
  } catch (error) {
    console.error(error);
    updateApiStatus(false);
  }
}

async function fetchRecipes() {
  try {
    const response = await fetch(`${API_BASE}/recipes`);
    if (!response.ok) throw new Error("Error al obtener recetas");
    const data = await response.json();
    renderRecipes(data);
  } catch (error) {
    console.error(error);
  }
}

function createInventoryRow(item) {
  const template = document.getElementById("inventory-row-template");
  const row = template.content.firstElementChild.cloneNode(true);
  row.dataset.itemId = item.id;
  row.querySelector(".inventory-name").textContent = item.product_name;
  row.querySelector(".quantity-input").value = item.quantity;
  row.querySelector(".unit").textContent = item.unit;
  if (item.expires_at) {
    row.querySelector(".date-input").value = item.expires_at;
  }
  if (item.notes) {
    row.querySelector(".notes-input").value = item.notes;
  }
  return row;
}

function renderInventory(products) {
  inventoryBody.innerHTML = "";
  const filter = inventorySearch.value.toLowerCase();
  products
    .filter((product) => product.product_name.toLowerCase().includes(filter))
    .forEach((product) => {
      const groupedRow = document.createElement("tr");
      groupedRow.classList.add("group-row");
      groupedRow.innerHTML = `
        <td>${product.product_name}</td>
        <td>${formatQuantity(product.total_quantity, product.unit)}</td>
        <td colspan="3">${product.reorder_threshold ? `Umbral: ${product.reorder_threshold} ${product.unit}` : ""}</td>
      `;
      inventoryBody.appendChild(groupedRow);

      product.items.forEach((item) => {
        const row = createInventoryRow(item);
        inventoryBody.appendChild(row);
      });
    });
}

function renderRecipes(recipes) {
  recipesList.innerHTML = "";
  if (recipes.length === 0) {
    recipesList.innerHTML = "<li>No hay recetas configuradas todavía.</li>";
    return;
  }

  recipes.forEach((recipe) => {
    const li = document.createElement("li");
    li.classList.add("recipe-card");
    li.innerHTML = `
      <div class="recipe-card__header">
        <h3>${recipe.name}</h3>
        <span class="recipe-card__status ${recipe.can_make ? "available" : "missing"}">
          ${recipe.can_make ? "Disponible" : "Faltan ingredientes"}
        </span>
      </div>
      <p>${recipe.description ?? ""}</p>
      <p><strong>Raciones:</strong> ${recipe.servings}</p>
      ${recipe.instructions ? `<p>${recipe.instructions}</p>` : ""}
      ${
        recipe.missing_ingredients.length
          ? `<div><p>Ingredientes faltantes:</p><ul class="missing-list">${recipe.missing_ingredients
              .map(
                (missing) =>
                  `<li>${missing.product_name}: ${missing.required_quantity} ${missing.unit} (disponible: ${missing.available_quantity})</li>`
              )
              .join("")}</ul></div>`
          : ""
      }
    `;
    recipesList.appendChild(li);
  });
}

async function handleManualEntry(event) {
  event.preventDefault();
  const formData = new FormData(manualEntryForm);
  const payload = [
    {
      product_name: formData.get("product"),
      quantity: Number(formData.get("quantity")),
      unit: formData.get("unit"),
      expires_at: formData.get("expires_at") || null,
      notes: formData.get("notes") || null,
    },
  ];

  try {
    const response = await fetch(`${API_BASE}/inventory/items`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload),
    });
    if (!response.ok) throw new Error("No se pudo registrar el alimento");
    manualEntryForm.reset();
    await fetchInventory();
    await fetchRecipes();
  } catch (error) {
    console.error(error);
    alert("Error al guardar el alimento. Revisa la consola para más detalles.");
  }
}

async function handleUpdateItem(button) {
  const row = button.closest("tr");
  const itemId = row.dataset.itemId;
  const quantityInput = row.querySelector(".quantity-input");
  const dateInput = row.querySelector(".date-input");
  const notesInput = row.querySelector(".notes-input");

  const payload = {
    quantity: Number(quantityInput.value),
    expires_at: dateInput.value || null,
    notes: notesInput.value || null,
  };

  try {
    const response = await fetch(`${API_BASE}/inventory/items/${itemId}`, {
      method: "PATCH",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload),
    });
    if (!response.ok) throw new Error("No se pudo actualizar el alimento");
    await fetchInventory();
    await fetchRecipes();
  } catch (error) {
    console.error(error);
    alert("Error al actualizar el alimento.");
  }
}

function renderReceiptItems(items) {
  receiptItemsBody.innerHTML = "";
  items.forEach((item, index) => {
    const row = document.createElement("tr");
    row.innerHTML = `
      <td><input type="text" name="product_${index}" value="${item.product_name}" required /></td>
      <td><input type="number" step="0.01" min="0" name="quantity_${index}" value="${item.quantity}" required /></td>
      <td>
        <select name="unit_${index}">
          <option value="unidad" ${item.unit === "unidad" ? "selected" : ""}>Unidad</option>
          <option value="kg" ${item.unit === "kg" ? "selected" : ""}>Kg</option>
          <option value="g" ${item.unit === "g" ? "selected" : ""}>Gramos</option>
          <option value="l" ${item.unit === "l" ? "selected" : ""}>Litros</option>
          <option value="ml" ${item.unit === "ml" ? "selected" : ""}>Mililitros</option>
        </select>
      </td>
      <td><button type="button" class="button small" data-action="remove" data-index="${index}">Eliminar</button></td>
    `;
    receiptItemsBody.appendChild(row);
  });
}

function showReceiptResults(result) {
  receiptItems = result.items;
  renderReceiptItems(receiptItems);
  receiptWarnings.textContent = result.warnings.join(" • ") || "";
  receiptResults.classList.toggle("hidden", receiptItems.length === 0);
}

async function parseReceiptText(event) {
  event.preventDefault();
  const text = new FormData(receiptTextForm).get("receipt_text");
  try {
    const response = await fetch(`${API_BASE}/inventory/receipt/parse-text`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ text }),
    });
    if (!response.ok) throw new Error("No se pudo interpretar el ticket");
    const data = await response.json();
    showReceiptResults(data);
  } catch (error) {
    console.error(error);
    alert("No se pudo interpretar el ticket. Comprueba el formato del texto.");
  }
}

async function scanReceipt(event) {
  event.preventDefault();
  const formData = new FormData(receiptScanForm);
  const file = formData.get("receipt_image");
  if (!file) {
    alert("Selecciona una imagen del ticket");
    return;
  }
  try {
    const response = await fetch(`${API_BASE}/inventory/receipt/scan`, {
      method: "POST",
      body: formData,
    });
    if (!response.ok) throw new Error("Error al procesar la imagen");
    const data = await response.json();
    showReceiptResults(data);
  } catch (error) {
    console.error(error);
    alert("No se pudo escanear el ticket. Asegúrate de que el backend tenga OCR disponible.");
  }
}

function removeReceiptItem(index) {
  receiptItems.splice(index, 1);
  renderReceiptItems(receiptItems);
  if (receiptItems.length === 0) {
    receiptResults.classList.add("hidden");
  }
}

async function commitReceiptItems(event) {
  event.preventDefault();
  if (receiptItems.length === 0) return;

  const updatedItems = receiptItems.map((item, index) => {
    const row = receiptItemsBody.children[index];
    return {
      product_name: row.querySelector(`input[name="product_${index}"]`).value,
      quantity: Number(row.querySelector(`input[name="quantity_${index}"]`).value),
      unit: row.querySelector(`select[name="unit_${index}"]`).value,
    };
  });

  try {
    const response = await fetch(`${API_BASE}/inventory/receipt/commit`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ items: updatedItems }),
    });
    if (!response.ok) throw new Error("No se pudo guardar el ticket");
    receiptResults.classList.add("hidden");
    receiptItems = [];
    receiptItemsBody.innerHTML = "";
    await fetchInventory();
    await fetchRecipes();
  } catch (error) {
    console.error(error);
    alert("Error al guardar los artículos del ticket");
  }
}

function setupTabs() {
  const buttons = document.querySelectorAll(".tab-button");
  const contents = document.querySelectorAll(".tab-content");

  buttons.forEach((button) => {
    button.addEventListener("click", () => {
      const target = button.dataset.tab;
      buttons.forEach((btn) => btn.classList.toggle("active", btn === button));
      contents.forEach((content) =>
        content.classList.toggle("active", content.dataset.content === target)
      );
    });
  });
}

inventoryBody.addEventListener("click", (event) => {
  const button = event.target.closest("button[data-action='save']");
  if (button) {
    handleUpdateItem(button);
  }
});

receiptItemsBody.addEventListener("click", (event) => {
  const button = event.target.closest("button[data-action='remove']");
  if (!button) return;
  removeReceiptItem(Number(button.dataset.index));
});

manualEntryForm.addEventListener("submit", handleManualEntry);
receiptTextForm.addEventListener("submit", parseReceiptText);
receiptScanForm.addEventListener("submit", scanReceipt);
receiptItemsForm.addEventListener("submit", commitReceiptItems);
inventorySearch.addEventListener("input", () => renderInventory(currentInventory));

setupTabs();
fetchInventory();
fetchRecipes();
