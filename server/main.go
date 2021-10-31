package main

import (
	"math/rand"
	"time"

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

type User struct {
	Name     string `json:"name"`
	Username string `json:"username"`
	Password string `json:"password"`
}

type LoginRequest struct {
	Username string `json:"username"`
	Password string `json:"password"`
}

type RegisterRequest struct {
	Name     string `json:"name"`
	Username string `json:"username"`
	Password string `json:"password"`
}

type AccountResponse struct {
	Name  string `json:"name"`
	Token string `json:"token"`
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

func init() {
	rand.Seed(time.Now().UnixNano())
}

var letterRunes = []rune("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

func RandStringRunes(n int) string {
	b := make([]rune, n)
	for i := range b {
		b[i] = letterRunes[rand.Intn(len(letterRunes))]
	}
	return string(b)
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

	users := map[string]User{}
	users["janet"] = User{"Janet Blackquill", "janet", "password"}

	sessions := map[string]string{}

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
	app.Post("/login", func(c *fiber.Ctx) error {
		var r LoginRequest
		err := c.BodyParser(&r)
		if err != nil {
			return err
		}

		u := users[r.Username]
		// TODO: check if user exists
		if u.Password != r.Password {
			return fiber.ErrUnauthorized
		}

		sessions[r.Username] = RandStringRunes(30)

		return c.JSON(AccountResponse{u.Name, sessions[r.Username]})
	})
	app.Post("/register", func(c *fiber.Ctx) error {
		var r RegisterRequest
		err := c.BodyParser(&r)
		if err != nil {
			return err
		}

		// TODO: check if user exists
		users[r.Username] = User{
			Name:     r.Name,
			Username: r.Username,
			Password: r.Password,
		}

		sessions[r.Username] = RandStringRunes(30)

		return c.JSON(AccountResponse{r.Name, sessions[r.Username]})
	})

	app.Listen(":25727")
}
