import { useMemo } from 'react';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';
import './ProductForm.css';

const schema = z.object({
  name: z.string().min(2, 'Introduce un nombre válido.'),
  category: z.string().min(2, 'Selecciona una categoría.'),
  stock: z.coerce.number().min(0, 'El stock debe ser igual o mayor a 0.'),
  unit: z.string().min(1, 'Indica una unidad (kg, unidades, etc.)'),
  expirationDate: z.string().optional()
});

export default function ProductForm({ onSubmit, isSubmitting }) {
  const {
    register,
    handleSubmit,
    reset,
    formState: { errors }
  } = useForm({
    resolver: zodResolver(schema),
    defaultValues: {
      name: '',
      category: '',
      stock: 0,
      unit: 'unidades',
      expirationDate: ''
    }
  });

  const errorMessages = useMemo(
    () =>
      Object.entries(errors).map(([key, value]) => (
        <li key={key}>{value.message}</li>
      )),
    [errors]
  );

  const submit = handleSubmit((data) => {
    onSubmit(data);
    reset();
  });

  return (
    <form className="product-form" onSubmit={submit} noValidate>
      <div className="form-grid">
        <label>
          Nombre
          <input type="text" placeholder="Ej. Tomate pera" {...register('name')} />
        </label>
        <label>
          Categoría
          <input type="text" placeholder="Ej. Verduras" {...register('category')} />
        </label>
        <label>
          Stock
          <input type="number" min="0" step="1" {...register('stock')} />
        </label>
        <label>
          Unidad
          <input type="text" placeholder="kg, unidades..." {...register('unit')} />
        </label>
        <label>
          Fecha de caducidad
          <input type="date" {...register('expirationDate')} />
        </label>
      </div>

      <button type="submit" disabled={isSubmitting}>
        {isSubmitting ? 'Guardando...' : 'Registrar producto'}
      </button>

      {errorMessages.length > 0 && <ul className="errors">{errorMessages}</ul>}
    </form>
  );
}
