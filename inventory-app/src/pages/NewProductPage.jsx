import ProductForm from '../components/ProductForm.jsx';
import { useInventory } from '../hooks/useInventory.js';

export default function NewProductPage() {
  const { createProduct } = useInventory({ pollInterval: false });

  return (
    <section>
      <header className="page-header">
        <h2>Registrar nuevo producto</h2>
        <p>Completa el formulario con la información del producto para añadirlo al inventario.</p>
      </header>
      <ProductForm onSubmit={createProduct.mutate} isSubmitting={createProduct.isPending} />
    </section>
  );
}
