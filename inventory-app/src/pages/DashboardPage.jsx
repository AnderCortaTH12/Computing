import InventorySummary from '../components/InventorySummary.jsx';
import InventoryTable from '../components/InventoryTable.jsx';
import { useInventory } from '../hooks/useInventory.js';

export default function DashboardPage() {
  const { data, isLoading, summary } = useInventory();

  return (
    <section>
      <header className="page-header">
        <h2>Dashboard de inventario</h2>
        <p>Consulta la disponibilidad en tiempo real y el estado general del inventario.</p>
      </header>

      <InventorySummary summary={summary.data} />
      <InventoryTable items={data ?? []} isLoading={isLoading} />
    </section>
  );
}
