import RecipeSuggestions from '../components/RecipeSuggestions.jsx';
import { useRecipes } from '../hooks/useRecipes.js';

export default function RecipesPage() {
  const { data, isLoading } = useRecipes();

  return (
    <section>
      <header className="page-header">
        <h2>Sugerencias de recetas</h2>
        <p>Descubre preparaciones que aprovechan los ingredientes con mayor rotación.</p>
      </header>
      <RecipeSuggestions recipes={data ?? []} isLoading={isLoading} />
    </section>
  );
}
