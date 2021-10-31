package main

import (
	"github.com/gofiber/fiber/v2"
	"github.com/gofiber/fiber/v2/middleware/cors"
)

type Node struct {
	ID       string `json:"id"`
	Content  Item   `json:"content"`
	Children []Node `json:"children"`
}

type Item struct {
	Label string `json:"text"`
}

type Study struct {
	Name  string `json:"name"`
	Tasks []Task `json:"tasks"`
	Tree  Node   `json:"tree"`
}

type Task struct {
	Text          string `json:"text"`
	CorrectAnswer string `json:"correctAnswer"`
}

func mkNode(id, label string, children ...Node) Node {
	return Node{
		ID:       id,
		Content:  Item{label},
		Children: children,
	}
}

func mkQuestion(text string, answer string) Task {
	return Task{
		Text:          text,
		CorrectAnswer: answer,
	}
}

func main() {
	app := fiber.New()

	var def = mkNode("homepage", "Homepage",
		mkNode("shop", "Shop"),
		mkNode("settings", "Settings"),
		mkNode("account", "My Account",
			mkNode("upgrade", "Upgrade my plan"),
			mkNode("profile", "Profile"),
			mkNode("balance", "Account balance")))

	var defQuestions = []Task{
		mkQuestion("Buy a jar.", "shop"),
		mkQuestion("Make the website gay.", "settings"),
		mkQuestion("Change your name.", "profile"),
	}

	var defStudy = Study{
		Name:  "Example Study",
		Tasks: defQuestions,
		Tree:  def,
	}

	app.Use(cors.New())

	data := map[string]Study{}
	data["demo"] = defStudy

	app.Get("/tree-test/:id", func(c *fiber.Ctx) error {
		v, ok := data[c.Params("id")]
		if !ok {
			return fiber.ErrNotFound
		}
		return c.JSON(v)
	})
	app.Post("/completed/tree-test/:id", func(c *fiber.Ctx) error {
		return nil
	})
	app.Get("/editor/tree-test/:id", func(c *fiber.Ctx) error {
		v, ok := data[c.Params("id")]
		if !ok {
			return fiber.ErrNotFound
		}
		return c.JSON(v)
	})
	app.Post("/editor/tree-test/:id", func(c *fiber.Ctx) error {
		var t Study
		err := c.BodyParser(&t)
		if err != nil {
			return err
		}
		data[c.Params("id")] = t
		return nil
	})

	app.Listen(":25727")
}
