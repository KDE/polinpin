package main

import (
	"github.com/gofiber/fiber/v2"
	"github.com/gofiber/fiber/v2/middleware/cors"
)

type Node struct {
	ID       string `json:"id"`
	Label    string `json:"label"`
	Children []Node `json:"children"`
}

type Question struct {
	Text          string   `json:"text"`
	CorrectAnswer []string `json:"correctAnswer"`
}

type TreeTest struct {
	Node      Node       `json:"node"`
	Questions []Question `json:"question"`
}

func mkNode(id, label string, children ...Node) Node {
	return Node{
		ID:       id,
		Label:    label,
		Children: children,
	}
}

func mkQuestion(text string, answer ...string) Question {
	return Question{
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

	var defQuestions = []Question{
		mkQuestion("Buy a jar.", "homepage", "shop"),
		mkQuestion("Make the website gay.", "homepage", "settings"),
		mkQuestion("Change your name.", "homepage", "account", "profile"),
	}

	var defTest = TreeTest{
		Node:      def,
		Questions: defQuestions,
	}

	app.Use(cors.New())

	app.Get("/tree-test/:id", func(c *fiber.Ctx) error {
		return c.JSON(defTest)
	})
	app.Post("/completed/tree-test/:id", func(c *fiber.Ctx) error {
		return nil
	})

	app.Listen(":25727")
}
