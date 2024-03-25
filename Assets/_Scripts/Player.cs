using UnityEngine;

public class Player : MonoBehaviour
{
    public Transform camera;
    public float horizontalinput;
    public float Verticalinput;
    public float speed = 2f;
    private float timer = 0;
    
    private Animator action;
    private Rigidbody rb;


    //public Animator jian_animator;



    public static Player instace = null;
    private void Awake()
    {
        instace = this;
        action = GetComponent<Animator>();
        rb = GetComponent<Rigidbody>();
    }

    float currentVel;
    float smoothtime = 0.2f;
    private void FixedUpdate()
    {
        
    }
    
    void Update()
    {
        if (timer > 0)
        {
            timer -= Time.deltaTime;
        }
        if (Input.GetKeyDown(KeyCode.Space) && timer <= 0)
        {
            Debug.Log("Jump");
            action.SetTrigger("Jump");
            rb.AddForce(transform.up * 200);
            timer = 0.8f;
        }

        if (Input.GetKey(KeyCode.LeftShift))
        {
            speed = 3;
        }
        if (Input.GetKeyUp(KeyCode.LeftShift))
        {
            speed = 2;
        }
        Vector2 input = new Vector2(Input.GetAxisRaw("Horizontal"), Input.GetAxisRaw("Vertical"));
        Vector2 dir = input.normalized;
        float targetAngle = Mathf.Atan2(input.x, input.y) * Mathf.Rad2Deg + camera.eulerAngles.y;

        if (dir != Vector2.zero)
        {
            transform.eulerAngles = Vector3.up * Mathf.SmoothDampAngle(transform.eulerAngles.y, targetAngle, ref currentVel, smoothtime);
            transform.Translate(transform.forward * Time.deltaTime * speed, Space.World);
        }
        
        if ((Input.GetKey(KeyCode.W) || Input.GetKey(KeyCode.A) || Input.GetKey(KeyCode.D) || Input.GetKey(KeyCode.S) )&& Input.GetKey(KeyCode.LeftShift))
        {
            action.SetBool("IsRun", true);
            action.SetBool("IsWalk", false);
            return;
        }
        if ((Input.GetKey(KeyCode.W) || Input.GetKey(KeyCode.A) || Input.GetKey(KeyCode.D) || Input.GetKey(KeyCode.S) )&& !Input.GetKey(KeyCode.LeftShift))
        {
            action.SetBool("IsWalk", true);
        }
        if (!(Input.GetKey(KeyCode.W) || Input.GetKey(KeyCode.A) || Input.GetKey(KeyCode.D) || Input.GetKey(KeyCode.S)))
        {
            action.SetBool("IsWalk", false);
        }

        if (!Input.GetKey(KeyCode.LeftShift))
        {
            action.SetBool("IsRun", false);
        }
    }
    public void StartUsing(float time)
    {
        Invoke("StartUsing",time);
        
    }
    public void StartUsing()
    {
        this.enabled = true;
        camera.gameObject.GetComponent<PlayerCamera>().enabled = true;
    }

    public void OverUsing(float time)
    {
        Invoke("OverUsing", time);

    }
    public void OverUsing()
    {
        this.enabled = false;
        camera.gameObject.GetComponent<PlayerCamera>().enabled = false;
    }

}
