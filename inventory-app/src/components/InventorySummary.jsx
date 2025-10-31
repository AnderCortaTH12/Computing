import './InventorySummary.css';

export default function InventorySummary({ summary }) {
  if (!summary) {
    return null;
  }

  const cards = [
    { label: 'Productos totales', value: summary.totalItems },
    { label: 'Stock bajo', value: summary.lowStock },
    { label: 'Categorías activas', value: summary.categories },
    { label: 'Valor del inventario', value: new Intl.NumberFormat('es-ES', { style: 'currency', currency: 'EUR' }).format(summary.inventoryValue ?? 0) }
  ];

  return (
    <section className="summary-grid" aria-label="Resumen del inventario">
      {cards.map((card) => (
        <article key={card.label} className="summary-card">
          <h3>{card.label}</h3>
          <p>{card.value ?? '—'}</p>
        </article>
      ))}
    </section>
  );
}
