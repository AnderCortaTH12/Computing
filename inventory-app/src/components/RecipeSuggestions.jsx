import './RecipeSuggestions.css';

export default function RecipeSuggestions({ recipes = [], isLoading }) {
  if (isLoading) {
    return <p>Buscando ideas de recetas...</p>;
  }

  if (!recipes.length) {
    return <p>No hay sugerencias disponibles por el momento.</p>;
  }

  return (
    <section className="recipes-grid" aria-label="Sugerencias de recetas">
      {recipes.map((recipe) => (
        <article key={recipe.id} className="recipe-card">
          <header>
            <h3>{recipe.title}</h3>
            {recipe.difficulty && <span className={`tag tag-${recipe.difficulty}`}>{recipe.difficulty}</span>}
          </header>
          <p>{recipe.description}</p>
          <div className="recipe-meta">
            <strong>Ingredientes disponibles:</strong>
            <ul>
              {(recipe.availableIngredients ?? []).map((ingredient) => (
                <li key={ingredient}>{ingredient}</li>
              ))}
            </ul>
          </div>
        </article>
      ))}
    </section>
  );
}
