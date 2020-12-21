import scala.annotation.tailrec

object Day21 {

  case class Food(ingredients: Set[String], allergens: Set[String]) {
    override def equals(that: Any): Boolean = that.isInstanceOf[Food] &&
      that.asInstanceOf[Food].allergens.equals(this.allergens) &&
      that.asInstanceOf[Food].ingredients.equals(this.ingredients)
  }

  def getFoodList: List[Food] = {
    Utils.getInputLines.map { line =>

      val parts = line.split(" \\(contains ")
      val ingredients = parts.head.split(" ")
      val allergens = parts.last.takeWhile(c => c != ')').split(", ")

      Food(ingredients.toSet, allergens.toSet)
    }
  }

  def ingredientsThatMayHaveAllergen(foods: List[Food], allergen: String): Set[String] = {
    foods.filter(f => f.allergens.contains(allergen))
      .map(f => f.ingredients)
      .reduce {
        _ intersect _
      }
  }

  def getPossibilitiesMap(foods: List[Food]): Map[String, Set[String]] = {
    foods.map(f => f.allergens).reduce {
      _ union _
    }.map(allergen => (allergen, ingredientsThatMayHaveAllergen(foods, allergen))).toMap
  }

  def countIngredientAppearances(ingredient: String, foods: List[Food]): Int =
    foods.map(f => f.ingredients).count(_.contains(ingredient))

  @tailrec
  def mapAllergenToIngredient(possibilities: Map[String, Set[String]], known: Map[String, String] = Map()): Map[String, String] = {
    if (possibilities.isEmpty) {
      known
    } else {
      val triviallyMapped = possibilities
        .filter { case (_, ingredients) => ingredients.size == 1 }
        .map { case (allergen, ingredients) => (allergen, ingredients.head) }
      val newKnown = triviallyMapped.foldLeft(known) { case (known, newPair) => known.updated(newPair._1, newPair._2) }
      val newPossibilities = possibilities.map {
        case (allergen, ingredients) => (allergen, ingredients.diff(triviallyMapped.values.toSet))
      }.filter {
        case (_, ingredients) => ingredients.nonEmpty
      }

      mapAllergenToIngredient(newPossibilities, newKnown)
    }
  }

  def main(args: Array[String]): Unit = {

    val foods = getFoodList
    val allergenPossibilities = getPossibilitiesMap(foods)
    val ingredients = foods.map(f => f.ingredients).reduce {
      _ union _
    }

    // Part 1
    val ingredientsWithoutAllergens = ingredients.diff(getPossibilitiesMap(getFoodList).values.reduce {
      _ union _
    })
    val appearances = ingredientsWithoutAllergens.toList.map(countIngredientAppearances(_, foods)).sum

    println(s"Part 1: $appearances appearances of ingredients without allergens.")

    // Part 2
    val allergenKnown = mapAllergenToIngredient(allergenPossibilities)
    val canonicalList = allergenKnown.toList.sortBy(pair => pair._1).map(_._2).mkString(",")
    println(s"Part 2: $canonicalList")
  }
}
