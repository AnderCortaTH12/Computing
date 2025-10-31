import { useQuery } from '@tanstack/react-query';
import { fetchRecipeSuggestions } from '../services/recipesService.js';

const RECIPES_KEY = ['recipes'];

export function useRecipes({ pollInterval = 30000 } = {}) {
  return useQuery({
    queryKey: RECIPES_KEY,
    queryFn: fetchRecipeSuggestions,
    refetchInterval: pollInterval
  });
}
