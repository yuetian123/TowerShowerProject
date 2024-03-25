using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Canvas : MonoBehaviour
{
    public CanvasGroup canvas;
    public bool useHidden = true;
    private Player player;
    private PlayerCamera camera;
    private bool isShow;
    private bool isHidden;

    private void Awake()
    {
        player = GameObject.FindGameObjectWithTag("Player").GetComponent<Player>();
        camera = player.camera.gameObject.GetComponent<PlayerCamera>();
    }

    private void OnEnable()
    {
        player.enabled = false;
        camera.enabled = false;
    }
    private void Start()
    {
        player.enabled = false;
        camera.enabled = false;
    }
    private void Update()
    {
        if (isShow)
        {
            canvas.alpha = Mathf.Lerp(canvas.alpha, 1, 0.2f);
            if (canvas.alpha >= 0.95f)
            {
                canvas.alpha = 1;
                isShow = false;
            }
        }
        if(isHidden)
        {
            canvas.alpha = Mathf.Lerp(canvas.alpha, 0, 0.2f);
            if (canvas.alpha <= 0.05f)
            {
                canvas.alpha = 0;
                isHidden = false;
                gameObject.SetActive(false);
            }
        }
        
    }

    private void OnDisable()
    {
        if (useHidden)
        {
            player.enabled = true;
            camera.enabled = true;
        }
    }
    public void Show()
    {
        canvas.alpha = 0;
        gameObject.SetActive(true);
        isShow = true;
    }
    public void hidden()
    {
        canvas.alpha = 1;
        isHidden = true;
    }
    
}
