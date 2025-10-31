import './InventoryTable.css';

export default function InventoryTable({ items = [], isLoading }) {
  if (isLoading) {
    return <p>Cargando inventario...</p>;
  }

  if (!items.length) {
    return <p>No hay productos registrados todavía.</p>;
  }

  return (
    <div className="table-wrapper">
      <table>
        <thead>
          <tr>
            <th>Producto</th>
            <th>Categoría</th>
            <th>Stock</th>
            <th>Unidad</th>
            <th>Última actualización</th>
          </tr>
        </thead>
        <tbody>
          {items.map((item) => (
            <tr key={item.id} className={item.status === 'low' ? 'row-low' : undefined}>
              <td>{item.name}</td>
              <td>{item.category}</td>
              <td>{item.stock}</td>
              <td>{item.unit}</td>
              <td>
                {item.updatedAt
                  ? new Intl.DateTimeFormat('es-ES', { dateStyle: 'medium', timeStyle: 'short' }).format(new Date(item.updatedAt))
                  : '—'}
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
