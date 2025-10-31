import { render, screen } from '@testing-library/react';
import InventorySummary from '../components/InventorySummary.jsx';

describe('InventorySummary', () => {
  it('muestra los valores del resumen', () => {
    render(
      <InventorySummary
        summary={{ totalItems: 42, lowStock: 5, categories: 8, inventoryValue: 1200.5 }}
      />
    );

    expect(screen.getByText(/productos totales/i)).toBeInTheDocument();
    expect(screen.getByText('42')).toBeInTheDocument();
    expect(screen.getByText(/stock bajo/i)).toBeInTheDocument();
    expect(screen.getByText('5')).toBeInTheDocument();
  });
});
