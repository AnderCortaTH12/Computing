import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import ProductForm from '../components/ProductForm.jsx';

describe('ProductForm', () => {
  it('envía los datos válidos', async () => {
    const handleSubmit = jest.fn();
    render(<ProductForm onSubmit={handleSubmit} isSubmitting={false} />);

    fireEvent.input(screen.getByLabelText(/nombre/i), { target: { value: 'Manzana' } });
    fireEvent.input(screen.getByLabelText(/categoría/i), { target: { value: 'Frutas' } });
    fireEvent.input(screen.getByLabelText(/stock/i), { target: { value: '10' } });
    fireEvent.input(screen.getByLabelText(/unidad/i), { target: { value: 'kg' } });

    fireEvent.click(screen.getByRole('button', { name: /registrar producto/i }));

    await waitFor(() => {
      expect(handleSubmit).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Manzana',
          category: 'Frutas',
          stock: 10,
          unit: 'kg'
        })
      );
    });
  });

  it('muestra mensajes de error cuando los campos son inválidos', async () => {
    const handleSubmit = jest.fn();
    render(<ProductForm onSubmit={handleSubmit} isSubmitting={false} />);

    fireEvent.click(screen.getByRole('button', { name: /registrar producto/i }));

    expect(await screen.findAllByText(/introduce un nombre válido/i)).toHaveLength(1);
    expect(handleSubmit).not.toHaveBeenCalled();
  });
});
