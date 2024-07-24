// --- Salary Operations ---
let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

// Filter high-income salaries
let highIncomeSalaries = List.filter (fun salary -> salary > 100000) salaries

// Calculate tax for all salaries based on the table
let Tax salary =
    match salary with
    | s when s <= 49020 -> s * 15 / 100
    | s when s <= 98040 -> s * 205 / 1000
    | s when s <= 151978 -> s * 26 / 100
    | s when s <= 216511 -> s * 29 / 100
    | _ -> salary * 33 / 100

let taxes = List.map Tax salaries

// Filter salaries less than $49,020 and add $20,000
let updatedSalaries = List.map (fun salary -> if salary < 49020 then salary + 20000 else salary) salaries

// Filter salaries between $50,000 and $100,000 and sum them
let middleIncomeSalaries = List.filter (fun salary -> salary >= 50000 && salary <= 100000) salaries
let sumMiddleIncomeSalaries = List.fold (+) 0 middleIncomeSalaries

printfn "=== Salary Operations ==="
printfn "Employee Salaries: %A\n" salaries
printfn "High Salaries: %A\n" highIncomeSalaries
printfn "Tax Amounts: %A\n" taxes
printfn "Added Salaries: %A\n" updatedSalaries
printfn "Sum of Middle Income Salaries: %d\n" sumMiddleIncomeSalaries
printfn "" 

// Function to sum multiples of 3 up to a given number `n`
let sumOfMultiplesOf3 n =
    let rec sumHelper acc current =
        if current > n then 
            acc
        else
            sumHelper (acc + current) (current + 3)
    let totalSum = sumHelper 0 3
    let numbersList = [3 .. 3 .. n]
    (totalSum, numbersList)


let result, numbers = sumOfMultiplesOf3 84


printfn "=== Tail Recursion Operation ==="
printfn "Numbers added: %A" numbers
printfn "Sum of all multiples of 3 up to 84: %d" result
